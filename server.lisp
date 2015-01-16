;; server part
(in-package :yomi)


;; Don't try to localize these variables
;; I am a nervous geek
(defvar *eval-threads-pool*
  (make-array
   *max-eval-threads*
   :initial-element nil))
(defvar *global-lock* (bt:make-lock))
(defvar *global-cv* (bt:make-condition-variable))



(defclass message ()
  ((command :initarg :command
	    :reader message-command)
   (data :initarg :data
	 :accessor message-data)))

;; websocket message commands
;; A Message is transfered as a json string

;; client -> server (requests)
;; 1. command: eval (or evalk, one sell evaluation by ctrl-return)  
;;    data:    a list of cell number, cell content pairs

;; 2. command: interrupt (interrupt)
;;    data:    emtpy string. what else you want?

;; 3. command: loadFile
;;    data:    filename (ex, "foo.yomi")

;; 4. command: saveFile (or saveFileWithCautio,n when the file already exists in the working directory, throws an error. Of course it is safe to save a file multiple times, if it's a loaded file or newly created file)
;;    data:    (filename cell-content-1 cell-content-2 ...)


;; server -> client (responses)
;; 1. command: evaled (or evaledk) 
;;    data: a list of (cellno, evaluated value, string for standard output)
;;     Evaluated value is again composed of a command and data
;;       a. command: text
;;          data:    string
;;       b. command: drawChart
;;          data:    an instance of chart class
;;       c. command: error
;;          data:    string

;; 2. command: code (code to load)
;;    data:    a list of string

;; 3. command: systemError (file IO/threads realated errors)
;;    data: a string


(defun start-yomi (&key
		     (working-directory *notebook-files-default-directory*)
		     (max-eval-threads *max-eval-threads*))
  
  ;; server preparation

  ;; Permission is not checked.
  ;; Maybe later
  (unless (directory-exists-p working-directory)
    (format t "~%No such directory in the system: ~A" working-directory)
    (format t "~%Exiting...")
    (return-from start-yomi))
  
  (setf *notebook-files-default-directory* working-directory)

  ;; I have no idea of "how many threads" is too many but
  ;; 100 seems to me too many.
  (unless (and (integerp max-eval-threads)
	       (> max-eval-threads 1)
	       (< max-eval-threads 100))
    (format t "~%max-eval-threads must be an integer from 2 to 99")
    (format t "~%Exiting...")
    (return-from start-yomi))

  (setf *max-eval-threads* max-eval-threads)
  

  (if *server-running-p*
      (format t "~%Server is already running at: http://localhost:~A/yomi"
	      *yomi-server-port*)
      (progn
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
	(open-browser *yomi-server-port*)

	)))

  

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
   until you find one.
  "
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



(defun vacant-room-for-thread? ()
  (let (result)
    (loop for x across *eval-threads-pool*
       for i from 0 do
	 (when (null x)
	   (setf result i)
	   (return)))
    result))


(defmethod resource-received-text ((res yomi-resource) client msg)
  (handler-case
      (let* ((msg (json:decode-json-from-string msg))
	     (command (cdr (assoc :command msg)))
	     (data (cdr (assoc :data msg))))
	(cond ((or (string= command "eval")
		   (string= command "evalk"))
	       ;; eval
	       (loop
		  ;; I am implementing semaphore here
		  ;; event loop 
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
								 (if (string= command "eval")
								     "evaled"
								     "evaledk")
								 (eval-code data))
						(bt:with-lock-held (*global-lock*)
						  (setf (aref *eval-threads-pool* room-no) nil)
						  (bt:condition-notify *global-cv*)))))))
				 (return))
			  (bt:condition-wait *global-cv* *global-lock*))))))

	      
	      ;; Works but ugly version of the following two conditionals
	      ;; ((or (string= command "saveFile")
	      ;; 	   (string= command "saveFileWithCaution"))
	      ;;  (let ((filename (string-trim '(#\space #\newline) (first data)))
	      ;; 	     (code (rest data)))
	      ;; 	 (macrolet ((include-supersede (&rest if-exists-supersede)
	      ;; 		      ;; no need to invoke gensym
	      ;; 		      `(with-open-file (s (merge-pathnames-as-file
	      ;; 					   *notebook-files-default-directory*
	      ;; 					   (make-pathname
	      ;; 					    :name filename
	      ;; 					    :type "yomi"))
	      ;; 					  :direction :output
	      ;; 					  ,@if-exists-supersede)
	      ;; 			 (json:encode-json code s)
	      ;; 			 (send-message client "systemMessage" "saved"))))
	      ;; 	   (if (string= command "saveFile")
	      ;; 	       (include-supersede :if-exists :supersede)
	      ;; 	       ;; "saveFileWithCaution"
	      ;; 	       ;; if file exists an error is thrown
	      ;; 	       (include-supersede)))))

	      ;; Stupid version of the above commented conditional
	      ((string= command "saveFile")
	       (let ((filename (string-trim '(#\space #\newline) (first data)))
		     (code (rest data)))
		 (with-open-file (s (merge-pathnames-as-file
				     *notebook-files-default-directory*
				     (make-pathname
				      :name filename
				      :type "yomi"))
				    :direction :output
				    :if-exists :supersede) ; the only difference
		   (json:encode-json code s)
		   (send-message client "systemMessage" "saved"))))
	      ;; throws an error if the file exists
	      ((string= command "saveFileWithCaution")
	       (let ((filename (string-trim '(#\space #\newline) (first data)))
		     (code (rest data)))
		 (with-open-file (s (merge-pathnames-as-file
				     *notebook-files-default-directory*
				     (make-pathname
				      :name filename
				      :type "yomi"))
				    :direction :output)
		   (json:encode-json code s)
		   (send-message client "systemMessage" "saved"))))
	      


	      ((string= command "loadFile")
	       (with-open-file (s (merge-pathnames-as-file
				   *notebook-files-default-directory*
				   data))
		 (send-message
		  client "code"
		  (json:decode-json s))))
	      

	      ((string= command "interrupt")
	       ;; stop all processes
	       ;; it means all evaluation requests from opened clients.
	       (loop for thread across *eval-threads-pool*
		  for i from 0 do
		    (when (and (bt:threadp thread)
			       (bt:thread-alive-p thread))
		      (bt:destroy-thread thread))
		  ;; nullify
		    (setf (aref *eval-threads-pool* i) nil))
	       (send-message client "systemMessage" "interrupted"))))

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




(defun eval-code (data)
  (loop
     for (cell-no cell-content) in data collect
       (let (evaled-value)
	 (let ((side-effect
		(handler-case 
		    (let ((standard-output
			   (with-output-to-string (*standard-output*)
			     (setf evaled-value
				   (eval-with-prelude cell-content)
				   ))))
		      standard-output)
		  
		  ;; only comments
		  ;; listize side effects when an error is thrown
		  ;; for comparison
		  (end-of-file () (list (format nil "")))
		  (error (c) (list (format nil "ERROR: ~A" c)))

		  )))
	   (cond ((listp side-effect)	; errored case
		  (list cell-no
			(make-instance 'message
				       :command "error"
				       :data (first side-effect))
			""))
		 
		 (t
		  (list cell-no
			(cond ((eql (type-of evaled-value) 'chart)
			       (make-instance 'message
					      :command "drawChart"
					      :data evaled-value))
			      
			      ;; All the rest are converted to strings
			      (t
			       (make-instance 'message
					      :command "text"
					      :data (format nil "~A" evaled-value))))
			side-effect)))))))


(defmethod resource-received-binary ((res yomi-resource) client message)
  (format t "got binary frame ~s from client ~s" (length message) client))



(defun enclose-code-with-prelude (str)
  (format nil "(progn (in-package :ynb)  ~A)" str))


;; todo
;; <- must be studied further
(defun eval-with-prelude (str)
  (let (result)
    (in-package :ynb)
    (setf result (eval (read-from-string (format nil "(progn ~A)" str))))
    (in-package :yomi)
    result))




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



(define-easy-handler (yomi-main :uri "/yomi") (yomifile)
  (let ((*attribute-quote-char* #\"))
    (if (and (not (null yomifile))
	     (= (length yomifile) 3))
	(notebook-page (second yomifile))
	(notebook-page))))


;; (start-yomi)

