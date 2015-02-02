(in-package :yomi)

;; A client is served by each 'workplace' most of time
(defstruct workplace client queue lock cv event-loop)

;; Dispatching (or dispatched if you will) functions are
;; registerd in a hash table.
(defvar *message-handling-function-set*
  (make-hash-table :test #'equal))

;; ========================================================
;; Utilities
;; A user can use the following commands(functions) in the browser.
;; ======================================================--
(defun cd (dir)
  "Change directory as you might have guessed."
  (let ((dir (fad:pathname-as-directory dir)))
    (if (fad:directory-exists-p dir)
	(setf *working-directory* dir)
	(error "~A doesn't exist" dir))))

(defun ls (&optional (dir *working-directory*))
  "List directory"
  (format nil "~{~S~^~%~}" (fad:list-directory dir)))

;; Need for a configuration file? Nah!
;; :) Every single serious Lisp programmer lives with Emacs, doesn't he?
(defun keymap (editor)
  "emacs, vim, and sublime keymaps are available, emacs is the default of course"
  (let ((editor (string-downcase editor)))
    (if (member editor (list "emacs" "vim" "sublime") :test #'equal)
	(progn (setf *keymap* editor)
	       (format nil "Keymap changed to ~A~%Relaod the page" editor))
	(error "Keymap must of one of ~S, ~S, or ~S, given ~A"
	       "emacs" "vim" "sublime" editor))))

(defun set-package (package)
  "A user can change the current working package"
  (let ((package (string-upcase (string package))))
    (if (find-package package)
	(progn
	  (setf *default-working-package* package)
	  ;; Send all clients to set package
	  (loop for w1 in *workplaces* do
	       (send-message (workplace-client w1)
			     "systemMessage" (list "set-package" package)))
	  (format nil "Package set to ~A" package))
	(error "Package ~A does not exist" package))))

;; ===============================================
;; QUEUE, scrapped from http://www.rosettacode.org
;; and adapted a bit.
;; ===============================================
(defstruct queue items tail)

(defun queue-empty-p (queue)
  "Returns true if the queue is empty."
  (endp (queue-items queue)))

(defun enqueue (item queue)
  "Enqueue item in queue. Returns the queue."
  (prog1 queue
    (if (queue-empty-p queue)
	(setf (queue-items queue) (list item)
	      (queue-tail queue) (queue-items queue))
	(setf (cdr (queue-tail queue)) (list item)
	      (queue-tail queue) (cdr (queue-tail queue))))))

(defun dequeue (queue)
  "Dequeues an item from queue. nil if queue is empty."
  (unless (queue-empty-p queue)
    (pop (queue-items queue))))

;; ==========================================================
;; Server -> Client
;; While a client-to-server message is just a simple list of strings,
;; a server-to-client message a bit more complex,
;; because you may want to draw a chart, to show some animations,
;; to play music or whatever in a browser evaluated results
;; Of course it is encoded as a JSON string as well.
;; ==========================================================

;; Since cl-json uses MOP it's just better to define it
;; as a class not a structure, 
;; don't bother with efficiencies, it doesn't matther at all here.
(defclass message ()
  ((command :initarg :command)
   (data :initarg :data)))

;; 1. command: evaled (or evaledkey, evaledon) 
;;    data: an evaled-result

;; 2. command: code (code to load)
;;    data:    a list of string

;; 3. command: systemError (file IO error, ...)
;;    data: a string

;; 4. command: systemMessage
;;    data: anything (You may think a string suffieces for this but
;;                    'set-package' requires a package name as well
;;                    and you never know what other kinds of system messages
;;                    you may want to provide later.)

(defclass evaled-result ()
  ((cellno :initarg :cellno)
   (type :initarg :type)
   (value :initarg :value)
   (stdout :initarg :stdout)))

;; * type : text
;;   value: string

;; * type : chart
;;   value:  a chart

;; Generate cells and write code there
;; * type : code
;;   value: a list of string

;; * type : error
;;   value: string

;; Sometimes you may want to render multiple results
;; in a single result area
;; vertical pack
;; e. type : vpack
;;    value: a list of instances of evaled-result
;;           (recursive, cellno is not necessary)

;; horizontal pack
;; f. type : hpack (horizontal pack)
;;    value: a list of instances of evaled-result
;;           (recursive, cellno is not necessary)

(defmacro handle-message ((client command data) &body body)
  `(setf (gethash ,command *message-handling-function-set*)
	 #'(lambda (,client ,data)
	     ,@body)))

;; data: (cell-no cell-content)
;; cell-no: integer
;; cell-content: string (code)
(handle-message (client "eval" data)
  (send-message client "evaled" (eval-cell data)))

;; evaluation by keyboard shortcut
(handle-message (client "evalkey" data)
  (send-message client "evaledkey" (eval-cell data)))

;; Clients may (unless this is not the last cell) send back
;; a message to evaluate the follow-up cells
(handle-message (client "evalon" data)
  (send-message client "evaledon" (eval-cell data)))

(defun savefile (client data &optional enforce)
  (let* ((filename (string-trim '(#\space #\newline) (first data)))
	 ;; full path
	 (filename-to-save
	  (fad:merge-pathnames-as-file
	   (fad:pathname-as-directory *working-directory*)
	   ;; ex) filename.yomi
	   (make-pathname :name filename :type "yomi"))))
    (flet ((savefile-supersede ()
	     (with-open-file (s filename-to-save
				:direction :output
				:if-exists :supersede)
	       (json:encode-json (rest data) s))))
      (cond (enforce (savefile-supersede)
		     (send-message client "systemMessage" "saved"))
	    (t (if (fad:file-exists-p filename-to-save)
		   (error "FILE EXISTS: ~A" filename-to-save)
		   (progn (savefile-supersede)
			  (send-message client "systemMessage" "saved"))))))))

(handle-message (client "saveFile" data)
  (savefile client data))

(handle-message (client "saveFileEnforce" data)
  (savefile client data t))

(handle-message (client "loadFile" data)
  (let* ((filename (first data))
	 (filename-full (fad:merge-pathnames-as-file
			 (fad:pathname-as-directory *working-directory*)
			 filename)))
    (cond ((not (fad:file-exists-p filename-full))
	   (error "~A not exists in ~A" filename *working-directory*))
	  (t (with-open-file (s filename-full)
	       (send-message
		client "code"
		(list filename (json:decode-json s))))))))

;; 'interrupt' message is special,
;; handle-message does not handle it.
;; Since handle-message macro is expanded and invoke a fuction
;; inside a event loop, attempting to send a signal to kill
;; the thread that runs the event loop causes an error.
;; So it must be handled in main thread.
;; See "resource-received-text"

;; ============================================
;; evaluate code
;; ============================================
(defun eval-cell (data)
  (destructuring-bind (cell-no cell-content) data
    (multiple-value-bind (result stdout) (eval-code cell-content)
      (build-evaled-result result stdout cell-no))))

(defun eval-code (code-string)
  (handler-case
      (let (result)
	(let ((stdout (with-output-to-string (*standard-output*)
			(setf result
			      (eval-with-prelude code-string)))))
	  (values result stdout)))
    (error (c) (values c ""))))

(defgeneric build-evaled-result (result stdout cell-no)
  (:documentation "make an instance of evaled-result based on the result evaluated"))

;; comments only content
(defmethod build-evaled-result ((result end-of-file) stdout cell-no)
  (make-instance 'evaled-result :cellno cell-no :type "error" :value "" :stdout stdout))

;; This is different from a system error
(defmethod build-evaled-result ((result error) stdout cell-no)
  (make-instance 'evaled-result
		 :cellno cell-no :type "error"
		 :value (format nil "ERROR: ~A" result)
		 :stdout stdout))

(defmethod build-evaled-result ((result chart) stdout cell-no)
  (make-instance 'evaled-result
		 :cellno cell-no :type "chart" :value result :stdout stdout))

(defmethod build-evaled-result ((result code) stdout cell-no)
  (make-instance 'evaled-result
		 :cellno cell-no :type "code"
		 :value (codelist result) :stdout stdout))

(defclass vpack ()
  ((elts :initarg :elts :reader pack-elts)))

(defclass hpack ()
  ((elts :initarg :elts :reader pack-elts)))

(defun vpack (&rest elts)
  (make-instance 'vpack :elts elts))

(defun hpack (&rest elts)
  (make-instance 'hpack :elts elts))

(defun packup (elts)
  (loop for elt in elts collect
       ;; cell no and stdout is meaningless here
       (build-evaled-result elt "" nil)))

(defmethod build-evaled-result ((result vpack) stdout cell-no)
  (make-instance 'evaled-result
		 :cellno cell-no :type "vpack"
		 :value (packup (pack-elts result)) :stdout stdout))

(defmethod build-evaled-result ((result hpack) stdout cell-no)
  (make-instance 'evaled-result
		 :cellno cell-no :type "hpack"
		 :value (packup (pack-elts result)) :stdout stdout))

(defclass rawtext ()
  ((content :initarg :content :reader content)))

(defmethod build-evaled-result ((result rawtext) stdout cell-no)
  ;; what stanard output?
  (declare (ignore stdout))
  (make-instance 'evaled-result
		 ;; Is ##rawtext too long? or informative?
		 :cellno cell-no :type "##rawtext"
		 :value (content result) :stdout "")) 

;; all the rest
(defmethod build-evaled-result ((result t) stdout cell-no)
  (make-instance 'evaled-result
		 :cellno cell-no :type "text"
		 :value (format nil "~A" result) :stdout stdout))

(defun eval-with-prelude (str)
  (let (result)
    (multiple-value-bind (cell-type rest) (detect-cell-type str)
      (cond ((string= cell-type "##rawtext")
	     (setf result (make-instance 'rawtext :content rest)))
	    ;; more cell-types? 
	    (t
	     #+SBCL
	     (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
	       (SETQ *PACKAGE*
		     (SB-INT:FIND-UNDELETED-PACKAGE-OR-LOSE *DEFAULT-WORKING-PACKAGE*)))
	     #+CCL
	     (EVAL-WHEN (:EXECUTE :LOAD-TOPLEVEL :COMPILE-TOPLEVEL)
	       (CCL::SET-PACKAGE *DEFAULT-WORKING-PACKAGE*))

	     (setf result
		   ;; handle mutiple values
		   (list-to-vpack
		    (multiple-value-list
		     (eval (read-from-string (format nil "(progn ~A)" str))))))
	     (in-package :yomi))))
    result))

(defun detect-cell-type (str)
  "cell type is defined at the beginning of the string with prefixed with
   two sharps and followed by alphabets like ##rawtext, spaces are left trimmed "
  (let ((str (string-left-trim '(#\space #\newline #\tab) str)))
    (multiple-value-bind (a b)
	(ppcre:scan "##[A-Za-z]+" str)
      (if (eql a 0)
	  (values (subseq str a b)
		  (string-left-trim '(#\space #\newline #\tab)
				    (subseq str b)))
	  (values "" "")))))

(defun list-to-vpack (xs)
  (case (length xs)
    ;; no value is just passed as nil
    (0 nil)
    (1 (first xs))
    (otherwise (apply #'vpack xs))))

;; ===================================================
;; Websocket
;; ===================================================
(defclass yomi-resource (ws-resource) ())

(defun start-event-loop (client lock cv queue)
  (bt:make-thread
   (let ((client client) (lock lock) (cv cv) (queue queue))
     #'(lambda ()
	 (loop (let (msg)
		 (bt:with-lock-held (lock)
		   (if (queue-empty-p queue)
		       (bt:condition-wait cv lock)
		       (setf msg (dequeue queue))))
		 ;; nil can never be a dequed message
		 (when msg
		   (handler-case
		       ;; dispatched by the first message
		       (funcall (gethash (first msg) *message-handling-function-set*)
				client (rest msg))
		     (json:json-syntax-error (c)
		       (send-message
			client "systemError" (format nil "INVALID YOMIFILE: ~A" c)))
		     (error (c)
		       (send-message
			client "systemError" (format nil "SYSTEM ERROR: ~A" c)))))))))))

(defmethod resource-client-connected ((res yomi-resource) client)
  (let ((cv (bt:make-condition-variable)) (lock (bt:make-lock)) (queue (make-queue)))
    (push (make-workplace :client client :queue queue :cv cv :lock lock
			  :event-loop (start-event-loop client lock cv queue))
	  *workplaces*)
    (format (load-time-value *standard-output*)
	    "~&got connection on repl server from ~s : ~s~%"
	    (client-host client) (client-port client))))

;; If the following method causes an error, you will see a very strange message.
;; Search (invoke-restart 'drop-connection) in buffer.lisp of clws package,
;; and SBCL (and CCL as well) throws an error that says there's no restart case as
;; "drop-connection". Of course it is there.
;; I have absolutely no idea why.
;; Anyway just make sure to cover all cases.
(defmethod resource-client-disconnected ((res yomi-resource) client)
  (format (load-time-value *standard-output*)
	  "~&Client disconnected from resource ~A: ~A~%"
	  res client)
  (let ((wp (find client *workplaces* :key #'workplace-client)))
    (when wp
      (let ((eloop (workplace-event-loop wp)))
	;; event loop is a thread and alive, it's save to just kill it theoretically
	;; but why take risk when there's practically no cost.
	(and (bt:threadp eloop)
	     (bt:thread-alive-p eloop)
	     (bt:destroy-thread eloop))
	(setf *workplaces* (remove client *workplaces* :key #'workplace-client))))))

(defmethod resource-received-text ((res yomi-resource) client msg)
  (let* ((wp (find client *workplaces* :key #'workplace-client)) (lock (workplace-lock wp))
	 (q (workplace-queue wp)) (cv (workplace-cv wp))
	 (eloop (workplace-event-loop wp)))
    (let ((decoded-msg (json:decode-json-from-string msg)))
      (cond ((string= "interrupt" (first decoded-msg))
	     ;; kill the event loop restart it
	     (bt:destroy-thread eloop)
	     (setf (workplace-event-loop wp)
		   (start-event-loop client lock cv q))
	     (send-message client "systemMessage" "interrupted"))
	    (t (bt:with-lock-held (lock)
		 (enqueue decoded-msg q)
		 ;; wake up the event loop
		 (bt:condition-notify cv)))))))

(defun send-message (client command data)
  (write-to-client-text
   client
   (json:encode-json-to-string
    (make-instance 'message :command command :data data))))

;; Binary data is not used...yet
;; (defmethod resource-received-binary ((res yomi-resource) client message)
;;   (format t "got binary frame ~s from client ~s" (length message) client))

;; ======================================================
;; Run hunchentoot server
;; ======================================================
(defun start-yomi ()
  ;; server preparation
  ;; If server is already running
  (when *server-running-p*
    (format t "~%Server is already running at: http://localhost:~A/yomi"
	    *yomi-server-port*)
    (return-from start-yomi))
  
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
    (if (and (listp yomifile) (> (length yomifile) 1))
	(notebook-page (second yomifile))
	(notebook-page))))


