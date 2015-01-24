(in-package :yomi)

(defvar *eval-threads-pool*
  (make-array
   *max-eval-threads*
   :initial-element nil))

(defvar *global-lock* (bt:make-lock))
(defvar *global-cv* (bt:make-condition-variable))

(defvar *message-handling-function-set*
  (make-hash-table :test #'equal))

;; ======================================================
;; Utilities
;; ======================================================
(defun cd (dir)
  (let ((dir (fad:pathname-as-directory dir)))
    (if (fad:directory-exists-p dir)
	(setf *working-directory* dir)
	(error "~A doesn't exist" dir))))


(defun ls (&optional (dir *working-directory*))
  (format
   nil
   "~{~S~^~%~}"
   (fad:list-directory dir)))

(defun keymap (editor)
  (let ((editor (string-downcase editor)))
    (if (member editor (list "emacs" "vim" "sublime") :test #'equal)
	(progn (setf *keymap* editor)
	       (format nil "Keymap changed to ~A~%Relaod the page" editor))
	(error "Keymap must of one of ~S, ~S, or ~S, given ~A"
	       "emacs" "vim" "sublime" editor))))


(defun set-package (package)
  (let ((package (string-upcase (string package))))
    (if (find-package package)
	(setf *default-working-package* package)
	(error "Package ~A does not exist" package))))

;; ======================================================
;; messages are transferd as JSON string
;; ======================================================

;; websocket message commands
;; A Message is transfered as a json string

;; client -> server (requests)
;; 1. command: eval (or evalk, one sell evaluation by ctrl-return)  
;;    data:    a list of cell number, cell content pairs

;; 2. command: interrupt (interrupt)
;;    data:    emtpy string. what else you want?

;; 3. command: loadFile
;;    data:    filename (ex, "foo.yomi")


;;  When the file already exists in the working directory, sends an error message.
;;  Of course it is safe to save a file multiple times,
;;  if it's a loaded file or newly created file
;; 4. command: saveFile (or saveFileWithCaution)
;;    data:    (filename cell-content-1 cell-content-2 ...)

(defclass message ()
  ((command :initarg :command)
   (data :initarg :data)))

;; server -> client (responses)
;; 1. command: evaled (or evaledk) 
;;    data: a list of instances of evaled-result

;; 2. command: code (code to load)
;;    data:    a list of string

;; 3. command: systemError (file IO ...)
;;    data: a string

;; 4. command: systemMessage
;;    data: a string


(defclass evaled-result ()
  ((cellno :initarg :cellno :initform nil)
   (type :initarg :type)
   (value :initarg :value)
   (stdout :initarg :stdout)))

;; SIX types of evaled-result
;; a. type : text
;;    value: string

;; b. type : chart
;;    value:  an instance of chart class

;; Generate cells and write code there, client sends back to
;; server a message to evaluate those cell contents
;; c. type : code 
;;    value: a list of string

;; d. type : error
;;    value: string

;; Sometimes you may want to render multiple results
;; in a single result area
;; vertical pack
;; e. type : packv
;;    value: a list of instances of evaled-result
;;           (recursive, cellno is not necessary)

;; horizontal pack
;; f. type : packh (horizontal pack)
;;    value: a list of instances of evaled-result
;;           (recursive, cellno is not necessary)

(defmacro handle-message ((client command data) &body body)
  `(setf (gethash ,command *message-handling-function-set*)
	 #'(lambda (,client ,data)
	     ,@body)))


;; data: a list of (cell-no cell-content)
;; cell-no: integer
;; cell-content: string (code)
(handle-message (client "eval" data)
  (loop
     ;; I am implementing semaphore here
     ;; event loop 
     (bt:with-lock-held (*global-lock*)
       (let ((room-no (vacant-room-for-thread?)))
	 (if room-no
	     (progn (setf (aref *eval-threads-pool* room-no)
			  (bt:make-thread
			   ;; rebinding variables-list
			   (let ((data data) (room-no room-no))
			     #'(lambda ()
				 (unwind-protect
				      (send-message client
						    "evaled"
						    ;; Crucial part, eval-code-list
						    (eval-code-list data))
				   (bt:with-lock-held (*global-lock*)
				     (setf (aref *eval-threads-pool* room-no) nil)
				     (bt:condition-notify *global-cv*)))))))
		    (return))
	     (bt:condition-wait *global-cv* *global-lock*))))))

;; It may look code duplication, but
;; you never know what's going to happen later,
;; Hope this is tolerable.
;; evaluation by keyboard shortcut
(handle-message (client "evalk" data)
  (loop
     (bt:with-lock-held (*global-lock*)
       (let ((room-no (vacant-room-for-thread?)))
	 (if room-no
	     (progn (setf (aref *eval-threads-pool* room-no)
			  (bt:make-thread
			   ;; rebinding variables
			   (let ((data data) (room-no room-no))
			     #'(lambda ()
				 (unwind-protect
				      (send-message client
						    "evaledk" ; <- the only difference
						    (eval-code-list data))
				   (bt:with-lock-held (*global-lock*)
				     (setf (aref *eval-threads-pool* room-no) nil)
				     (bt:condition-notify *global-cv*)))))))
		    (return))
	     (bt:condition-wait *global-cv* *global-lock*))))))

;; helper
(defun vacant-room-for-thread? ()
  (let (result)
    (loop for x across *eval-threads-pool*
       for i from 0 do
	 (when (null x)
	   (setf result i)
	   (return)))
    result))


(handle-message (client "saveFile" data)
  (let ((filename (string-trim '(#\space #\newline) (first data)))
	(code (rest data)))
    (with-open-file (s (merge-pathnames-as-file
			(fad:pathname-as-directory *working-directory*)
			(make-pathname
			 :name filename
			 :type "yomi"))
		       :direction :output
		       :if-exists :supersede) ; the only difference
      (json:encode-json code s)
      (send-message client "systemMessage" "saved"))))

(handle-message (client "saveFileWithCaution" data)
  (let ((filename (string-trim '(#\space #\newline) (first data)))
	(code (rest data)))
    (with-open-file (s (merge-pathnames-as-file
			(fad:pathname-as-directory *working-directory*)
			(make-pathname
			 :name filename
			 :type "yomi"))
		       :direction :output)
      (json:encode-json code s)
      (send-message client "systemMessage" "saved"))))

(handle-message (client "loadFile" data)
  (with-open-file (s (merge-pathnames-as-file
		      (fad:pathname-as-directory *working-directory*)
		      data))
    (send-message
     client "code"
     (json:decode-json s))))

(handle-message (client "interrupt" data)
  (declare (ignore data client))
  (loop for thread across *eval-threads-pool*
     for i from 0 do
       (when (and (bt:threadp thread)
		  (bt:thread-alive-p thread))
	 (bt:destroy-thread thread))
     ;; nullify
       (setf (aref *eval-threads-pool* i) nil))
  
  ;; interrupt message must be sent to all clients
  (loop for c1 in *notebook-clients* do
       (send-message
	c1
	"systemMessage" "interrupted")))

;; ============================================
;; evaluate code
;; ============================================
(defun eval-code-list (data)
  (loop
     for (cell-no cell-content) in data collect
       (multiple-value-bind (result stdout) (eval-code cell-content)
	 (make-evaled-result result stdout cell-no))))

(defun eval-code (code-string)
  (handler-case
      (let (result)
	(let ((stdout (with-output-to-string (*standard-output*)
			(setf result
			      (eval-with-prelude code-string)))))
	  (values result stdout)))
    (error (c) (values c ""))))

(defgeneric make-evaled-result (result stdout cell-no)
  (:documentation "make an instance of evaled-result based on result evaluated"))

;; comments only content
(defmethod make-evaled-result ((result end-of-file) stdout cell-no)
  (make-instance 'evaled-result
		 :cellno cell-no
		 :type "error"
		 :value ""
		 :stdout stdout))

(defmethod make-evaled-result ((result error) stdout cell-no)
  (make-instance 'evaled-result
		 :cellno cell-no
		 :type "error"
		 :value (format nil "ERROR: ~A" result)
		 :stdout stdout))

(defmethod make-evaled-result ((result chart) stdout cell-no)
  (make-instance 'evaled-result
		 :cellno cell-no
		 :type "chart"
		 :value result
		 :stdout stdout))

(defmethod make-evaled-result ((result code) stdout cell-no)
  (make-instance 'evaled-result
		 :cellno cell-no
		 :type "code"
		 :value (codelist result)
		 :stdout stdout))


(defclass packv ()
  ((elts :initarg :elts
	 :reader pack-elts)))

(defclass packh ()
  ((elts :initarg :elts
	 :reader pack-elts)))

(defun packv (&rest elts)
  (make-instance
   'packv :elts elts))

(defun packh (&rest elts)
  (make-instance
   'packh :elts elts))

(defun packup (elts)
  (loop for elt in elts collect
       ;; cell no and stdout is meaningless here
       (make-evaled-result elt "" nil)))

(defmethod make-evaled-result ((result packv) stdout cell-no)
  (make-instance 'evaled-result
		 :cellno cell-no
		 :type "packv"
		 :value (packup (pack-elts result))
		 :stdout stdout))

(defmethod make-evaled-result ((result packh) stdout cell-no)
  (make-instance 'evaled-result
		 :cellno cell-no
		 :type "packh"
		 :value (packup (pack-elts result))
		 :stdout stdout))

;; all the rest
(defmethod make-evaled-result ((result t) stdout cell-no)
  (make-instance 'evaled-result
		 :cellno cell-no
		 :type "text"
		 :value (format nil "~A" result)
		 :stdout stdout))


(defun eval-with-prelude (str)
  (let (result)
    #+SBCL
    (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
      (SETQ *PACKAGE*
	    (SB-INT:FIND-UNDELETED-PACKAGE-OR-LOSE *DEFAULT-WORKING-PACKAGE*)))
    #+CCL
    (EVAL-WHEN (:EXECUTE :LOAD-TOPLEVEL :COMPILE-TOPLEVEL)
      (CCL::SET-PACKAGE *DEFAULT-WORKING-PACKAGE*))

    (setf result (eval (read-from-string (format nil "(progn ~A)" str))))
    (in-package :yomi)
    result))

;; ===================================================
;; Websocket
;; ===================================================
(defclass yomi-resource (ws-resource) ())

(defmethod resource-client-connected ((res yomi-resource) client)
  ;; Assume only one client allowed, no broadcasting
  (push client *notebook-clients*)
  (format (load-time-value *standard-output*)
	  "got connection on repl server from ~s : ~s~%"
	  (client-host client) (client-port client)))

(defmethod resource-client-disconnected ((res yomi-resource) client)
  (setf *notebook-clients* (remove client *notebook-clients*))
  (format (load-time-value *standard-output*)
	  "Client disconnected from resource ~A: ~A~%"
	  res client))

(defmethod resource-received-text ((res yomi-resource) client msg)
  (handler-case
      (let* ((msg (json:decode-json-from-string msg))
	     (command (cdr (assoc :command msg)))
	     (data (cdr (assoc :data msg))))
	(funcall (gethash command *message-handling-function-set*) client data))
    (json:json-syntax-error (c)
      (send-message
       client
       "systemError"
       (format nil "INVALID YOMIFILE: ~A" c)))
    (error (c)
      (send-message
       client
       "systemError"
       (format nil "SYSTEM ERROR: ~A" c)))))


(defun send-message (client command data)
  (write-to-client-text
   client
   (json:encode-json-to-string
    (make-instance 'message
		   :command command
		   :data data))))

;; Binary data is not used...yet
;; (defmethod resource-received-binary ((res yomi-resource) client message)
;;   (format t "got binary frame ~s from client ~s" (length message) client))

;; ======================================================
;; Run hunchentoot server
;; ======================================================
(defun start-yomi (&key
		     (working-directory
		      (fad:pathname-as-directory
		       *default-pathname-defaults*))
		     (max-eval-threads *max-eval-threads*))

  ;; server preparation
  ;; If server is already running
  (when *server-running-p*
    (format t "~%Server is already running at: http://localhost:~A/yomi"
	    *yomi-server-port*)
    (return-from start-yomi))
  
  ;; Permission is not checked.
  ;; Maybe later
  (unless (directory-exists-p working-directory)
    (format t "~%No such directory in the system: ~A" working-directory)
    (format t "~%Exiting...")
    (return-from start-yomi))
  
  (setf *working-directory* working-directory)

  ;; I have no idea of "how many threads" is too many but
  ;; 100 seems to me too many.
  (unless (and (integerp max-eval-threads)
	       (> max-eval-threads 1)
	       (< max-eval-threads 100))
    (format t "~%max-eval-threads must be an integer from 2 to 99")
    (format t "~%Exiting...")
    (return-from start-yomi))

  (setf *max-eval-threads* max-eval-threads)

  ;; All set now
  (setf *server-running-p* t)

  ;; find available ports 
  (setf *yomi-server-port* (search-available-port *yomi-server-port*))
  (setf *web-socket-port* (search-available-port *web-socket-port*))

  ;; websocket
  (bt:make-thread (lambda () (run-server *web-socket-port*))
		  :name "websockets server")
  (register-global-resource
   *ws-loc*
   (make-instance 'yomi-resource)
   (origin-prefix "http://127.0.0.1" "http://localhost"))

  (bt:make-thread (lambda ()
		    (run-resource-listener
		     (find-global-resource *ws-loc*)))
		  :name "resource listener")
	
  ;; Hunchentoot server
  (publish-static-content)
  (start (make-instance 'easy-acceptor :port *yomi-server-port*))
  (open-browser *yomi-server-port*))


(defun shorten-file-name (file)
  (format nil "/~A.~A" (pathname-name file)
	  (pathname-type file)))


(defun publish-static-content ()
  ;; initialize, may not be necessary.
  (setf *dispatch-table* '(DISPATCH-EASY-HANDLERS))
  (loop for file in (append *js-css-files* *image-files*) do
       (push (create-static-file-dispatcher-and-handler
	      (shorten-file-name file)
	      (merge-pathnames-as-file *yomi-path-default* file))
	     *dispatch-table*)))


(defun open-browser (port)
  #+(AND DARWIN SBCL)
  (sb-ext:run-program
   "/usr/bin/open"
   (list (format nil "http://localhost:~A/yomi" port)))
  
  #+(AND DARWIN CCL)
  (inferior-shell:run
   `(open ,(format nil "http://localhost:~A/yomi" port)))
    
  #+LINUX
  (inferior-shell:run
   `(xdg-open ,(format nil "http://localhost:~A/yomi" port)))

  #+OS-WINDOWS
  ;; haven't tested yet
  (inferior-shell:run
   `(start ,(format nil "http://localhost:~A/yomi" port)))
  
  ;; do nothing otherwise
  )


(defun search-available-port (&optional (from 8888))
  "If the given port number is already in use try another one
   until you find one."
  (flet ((in-use? (n)
	   (handler-case
	       (let ((socket (usocket:socket-listen "localhost" n)))
		 (usocket:socket-close socket)
		 nil)
	     (usocket:address-in-use-error () t))))
    (loop
       with port = from
       do
	 (if (in-use? port)
	     (progn
	       (format t "~%Port: ~A already in use.. trying next" port)
	       (incf port))
	     (progn
	       (format t "~%Found Avaiable Port Number: ~A" port)
	       (return port))))))


(define-easy-handler (yomi-main :uri "/yomi") (yomifile)
  (let ((*attribute-quote-char* #\"))
    (if (and (not (null yomifile))
	     (= (length yomifile) 3))
	(notebook-page (second yomifile))
	(notebook-page))))

