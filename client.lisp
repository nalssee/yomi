(in-package :yomi)

;; websocket message commands
;; A Message is transfered as a json string

;; client -> server (requests)
;; 1. command: eval (or evalk, one sell evaluation by ctrl-return)  
;;    data:    a list of cell number, cell content pairs

;; 2. command: interrupt (interrupt)
;;    data:    emtpy string. what else you want?

;; 3. command: loadFile
;;    data:    filename (ex, "foo.yomi")

;; 4. command: saveFile
;;    data:    (filename cell-content-1 cell-content-2 ...)


;; server -> client (responses)
;; 1. command: evaled (evaluated results)
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


(defun js-for-notebook-page (notebook-filename)
  (ps
    (lisp *ps-lisp-library*)

    (setf

     ;; string formatting
     ;; "<p>{0} {1}</p>".format("Hello", "Kenjin")
     ;; "<p>Hello Kenjin</p>
     (chain |String| prototype format)
     #'(lambda ()
	 (let ((s this)
	       (i arguments.length))
	   (while (>= i 0)
	     (setf s (s.replace (new (|RegExp| (+ "\\{" i "\\}") "gm")) (getprop arguments i)))
	     (decf i))
	   s)))

    ;; parameters
    ;; I have no idea of the nameing conventions for Parenscript global variables.
    (defvar focused-cell nil)
    (defvar all-cells (array))

    ;; removed cell contents to recover
    ;; a list of cell position and editor value pairs
    (defvar removed-cell-contents (array))
   
    (defvar cell-counter 0)


    ;; 0 : never been saved with the current file name
    ;; 1 : saved at least once
    (defvar ever-been-saved 0)

    (defun open-in-new-tab (url)
      (setf win (window.open url "_blank"))
      (win.focus))
    
   
    (defun choose-notebook ()
      (chain document (get-element-by-id "choose-notebook") (click)))


    (defun position (x xs)
      (let ((result nil))
	(loop for i from 0
	   for xi in xs do
	     (when (= x xi)
	       (setf result i)
	       (return)))
	result))
   
   
    ;; insert-after
    (defun insert-a (new-node reference-node)
      (chain reference-node
	     parent-node
	     (insert-before new-node (chain reference-node next-sibling))))

   
    ;; insert-before
    (defun insert-b (new-node reference-node)
      (chain reference-node
	     parent-node
	     (insert-before new-node reference-node)))

    
    ;; command can be either "eval" or "evalk"
    ;; "evalk" means evaluation by keyboard short cut (ctrl-return)
    ;; see for yourself what's the difference
    (defun eval-cell-and-go (cell &optional (command "eval"))

      (let ((cell-content (chain (getprop cell 'editor) (get-value)))
	    (cell-no (cell-position cell)))
	(when (not (and (last-cell-p cell)
			(= cell-content "")))
	  (ws.send
	   (chain +JSON+ (stringify
			  (make-message
			   command
			   (array
			    (array cell-no cell-content)))))))))

    
    (defun interrupt ()
      (ws.send
       (chain +JSON+ (stringify
		      (make-message
		       "interrupt"
		       "")))))

   
    ;; eval all cells from the focused cell
    (defun eval-forward ()

      (let* (
	     (cells-to-eval (chain all-cells
				   (slice (cell-position focused-cell))))
	     (first-cell-position (cell-position focused-cell)))
	(ws.send
	 (chain +JSON+ (stringify
			(make-message
			 "eval"
			 (loop for c in cells-to-eval
			    for i from first-cell-position collect
			      (array i (chain (getprop c 'editor) (get-value))))))))))

    
   
   
    (defun last-cell ()
      (let* ((n (chain all-cells length)))
	(getprop all-cells (- n 1))))

    (defun first-cell ()
      (getprop all-cells 0))
   

    (defun last-cell-p (cell)
      (= (getprop (last-cell) 'no)
	 (getprop cell 'no)))

    (defun empty-cell-p (cell)
      (= (chain (getprop cell 'editor) (get-value))
	 ""))
    
    (defun focus-to-next-cell (cell)
      (let ((cell-to-focus
	     ;; if the given cell is the last cell and the content is empty
	     ;; then the given cell is to be focused,
	     ;; if it is simply the last cell then make a new cell at the end
	     ;; and focus it to the newly made cell
	     ;; or else the next cell is focused
	     (cond ((and (last-cell-p cell)
			 (empty-cell-p cell))

		    cell)
		   ((last-cell-p cell)

		    (make-cell))
		   (t (getprop all-cells (1+ (cell-position cell)))))))

	(get-it-focused cell-to-focus)))


    ;; returns an integer
    (defun cell-position (cell)
      (position (getprop cell 'no)
		(mapcar (lambda (c) (getprop c 'no)) all-cells)))
    

   
    (defun turn-on-border (cell)
      (let ((divmain (getprop cell 'div-main)))
	(setf (chain divmain style border-color) "green")))
   
    (defun turn-off-border (cell)
      (when cell
	(let* ((divmain (getprop cell 'div-main)))
	  (setf (chain divmain style border-color) "#FFFFFF"))))
   
   
    ;; when the cell (actually div-outer) is clicked
    (defun get-it-focused (cell)
      (turn-off-border focused-cell)
      (setf focused-cell cell)
      (turn-on-border cell)
      )


    ;; add a cell after the focused cell
    (defun add-cell ()
      (make-cell focused-cell)
      )

      
    ;; remove a focused-cell and focus goes up
    ;; if there's only one cell do nothing
    (defun remove-cell ()
      (when (> (chain all-cells length) 1)
	
	(let ((div-outer (getprop focused-cell 'div-outer))
	      (n (cell-position focused-cell)))
	  (chain removed-cell-contents
		 (push (array n
			      (chain
			       (getprop (getprop all-cells n) 'editor) (get-value)))))
	  (chain all-cells (splice n 1))
	  (chain div-outer (remove))
	  (get-it-focused
	   (getprop all-cells
		    (if (= n 0)
			0
			(- n 1))))))
      
      )

    ;; recover the last removed cell
    (defun undo ()
      (when (> (chain removed-cell-contents length) 0)
	(let* ((to-recover (chain removed-cell-contents (pop)))
	       (n (getprop to-recover 0))
	       (content (getprop to-recover 1)))

	  (if (= n (chain all-cells length))
	      ;; last cell is removed
	      (let ((cell (make-cell)))
		(chain (getprop cell 'editor) (get-doc) (set-value content)))
	      (let ((cell (make-cell (getprop all-cells n))))
		(chain (getprop cell 'editor) (get-doc) (set-value content))
		(move-up))))))
    


    ;; scroll to focused element to show up
    (defun auto-scroll ()
      (chain (getprop focused-cell 'div-outer) (scroll-into-view)))
    
    (defun move-up ()
      (let* ((n (cell-position focused-cell))
	     (temp (getprop all-cells (1- n))))
	(when (> n 0)
	  (insert-b (getprop focused-cell 'div-outer)
		    (getprop temp 'div-outer))
	  (setf (getprop all-cells (1- n)) focused-cell)
	  (setf (getprop all-cells n) temp)
	  (auto-scroll)
	  )))
    
    
    (defun move-down ()
      (when (not (last-cell-p focused-cell))
	(let* ((n (cell-position focused-cell))
	       (temp (getprop all-cells (1+ n))))
	  (insert-b (getprop temp 'div-outer)
		    (getprop focused-cell 'div-outer))
	  (setf (getprop all-cells (1+ n)) focused-cell)
	  (setf (getprop all-cells n) temp)
	  (auto-scroll))))
    

    ;; 
    (defun rename-input-show-up ()
      (let ((span (chain document (get-element-by-id "rename_span")))
	    (input (chain document (get-element-by-id "rename_input"))))
	;; hide the file name span
	(setf (chain span style display)  "none")
	;; show input and button
	(setf (chain input style display) "inline")
	

	)
      
      
      )

    ;; blink rename_span content for a second.
    (defun blink-rename_span (message)
      (let* ((span (chain document (get-element-by-id "rename_span")))
	     (span-content (chain span |innerHTML|)))
	(setf (chain span |innerHTML|) message)
	(set-timeout (lambda ()
		       ;; revert it back
		       (setf (chain span |innerHTML|) span-content))
		     1000)))
    
    
    (defun rename-notebook ()
      (let ((span (chain document (get-element-by-id "rename_span")))
	    (input  (chain document (get-element-by-id "rename_input"))))
	(unless (= (chain span |innerHTML|)
		   (chain input value))
	  (setf ever-been-saved 0))
	
	(setf (chain span |innerHTML|)
	      (chain input value))
	(setf (chain span style display)  "inline")
	(setf (chain input style display) "none")
	

	
	)
      

      )

    ;; make a message to send to the server throught web socket
    (defun make-message (command data)
      (create command command
	      data data))
    
    (defun empty-notebook-p ()
      (and (= (chain all-cells length) 1)
	   (= ""
	      (chain (getprop (first-cell) 'editor) (get-value)))))
        
    (defun save-notebook ()
      (unless (empty-notebook-p)
	(let ((filename
	       (chain document (get-element-by-id "rename_span")
		      |innerHTML|)))
	  (ws.send 
	   (chain +JSON+
		  (stringify
		   (make-message
		    (if (= ever-been-saved 0)
			"saveFileWithCaution"
			"saveFile")
		    ;; first element of data list is a filename to save
		    (append (list filename)
			    (loop for cell in all-cells collect
				 (chain (getprop cell 'editor) (get-value)))))))))))

    
    

    ;; cell is an object with a div element and an editor in the element
    ;; if a sibling is given, make a cell after sibling
    ;; or make a cell as the first child of cellpad
    (defun make-cell (&optional sibling)
      (let ((div-outer (chain document (create-element "div")))
	    (div-main (chain document (create-element "div") ))
	    ;; p1, p2, p3 are solely for eye pleasure
	    (p1 (chain document (create-element "p")))
	    (textarea (chain document (create-element "textarea")))
	    (p2 (chain document (create-element "p")))
	    (resultarea (chain (chain document (create-element "div"))))
	    (p3 (chain document (create-element "p"))))
	
	;; style
	(setf (chain div-main style border-style) "solid")
	(setf (chain div-main style border-radius) "0.5em")
	(setf (chain div-main style border-color) "#FFFFFF")
	
	;; invisible border
	(setf (chain div-main style border-width) "1px")

	
	;; resultarea style
	(setf (chain resultarea style white-space) "pre-wrap")
	(setf (chain resultarea style margin-left) "50px")	
	
	;; append elements
	(chain div-main (append-child p1))
	(chain div-main (append-child textarea))
	(chain div-main (append-child p2))	
	(chain div-main (append-child resultarea))
	;; insert div-main into div-outer
	(chain div-outer (append-child div-main))
	(chain div-outer (append-child p3))

	
	;; You must first attach a div element to a document before it wears CodeMirror.
	(if (= sibling undefined)
	    (progn
	      (setf cellpad (chain document (get-element-by-id "cellpad")))
	      (chain cellpad (append-child div-outer)))
	    (insert-a div-outer (getprop sibling 'div-outer)))
	
	;; textarea wears CodeMirror
	(setf editor
	      (|CodeMirror|
	       (lambda (elt)
		 (chain textarea parent-node (replace-child elt textarea)))
	       (create value (chain textarea |innerHTML|)
		       mode "commonlisp"
		       line-numbers true
		       match-brackets true
		       viewport-margin |Infinity|)))
	
	;; disable ctrl-enter in the editor
	;; it is going to be used as cell evaluation
	;; if you don't include the following expression
	;; the editor will insert a newline for ctrl-enter
	(chain editor (set-option
		       "extraKeys"
		       (create "Ctrl-Enter"
			       (lambda (cm)))))

	
	;; set id to div-outer for later use
	(defvar cell-id (+ "cell" cell-counter))
	(setf (chain div-outer id) cell-id)
	
	;; Just include them all for convenience
	(let ((cell (create no cell-counter
			    id cell-id
			    div-outer div-outer
			    div-main div-main
			    result-area resultarea
			    editor editor)))
	  
	  ;; increase cell-counter by 1
	  (incf cell-counter)
	  ;; the cell just made gets focused
	  (get-it-focused cell)
	  ;; It's really tedious to carry the whole cells separately
	  ;; but I've got no better way yet.
	  (auto-scroll)

	  
	  (if (= sibling undefined)
	      (chain all-cells (push cell))
	      (chain all-cells (splice (1+ (cell-position sibling)) 0 cell)))
	  

	  ;; add event to div-outer
	  ;; when div-outer is clicked the cell is focused
	  (setf (chain div-outer onclick)
		(lambda () (get-it-focused cell)))
	  
	  ;; finally return the cell
	  cell)
	
	))

   
    ;; later you may want to add options parameter
    (defun draw-chart (place data options)
      (chain ($ document)
	     (ready (lambda ()
		      (chain $ (plot place data options))))))



    
    (defun cutout-extension (filename)
      (let ((splited  (chain filename (split "."))))
	(if (= (chain splited length) 1)
	    (getprop splited 0)
	    (chain splited (splice 0 (- (chain splited length) 1)) (join ".")))))
    
    
    (defun load-notebook-file (notebook-filename)
      (setf ever-been-saved 1)
      (when notebook-filename
	
	(ws.send
	 (chain +JSON+ (stringify
			(make-message
			 "loadFile"
			 notebook-filename))))
	
	
	(setf (chain document (get-element-by-id "rename_span")  |innerHTML|)
	      (cutout-extension notebook-filename))
	(setf (chain document (get-element-by-id "rename_input") value)
	      (cutout-extension notebook-filename)))

      
      
      

      )
    ;; chage title to notebook name

    
    (defun change-title ()
      (let ((title-element (chain document (get-element-by-id "title"))))
	(setf (chain title-element |innerHTML|)
	      (+ "YNB:"
		 (chain document (get-element-by-id "rename_input") value)))))
    

    
    (defun first (x) (getprop x 0))
    (defun rest (x) (chain x (slice 1)))

    (defun last1 (x)
      (getprop x (- (chain x length) 1)))
    
    (defun handle-message (msg)
      (let* ((json-msg (chain +JSON+ (parse msg)))
	     (command (chain json-msg command))
	     (data (chain json-msg data)))
	(cond
	  ((or (= command "evaled")
	       (= command "evaledk"))
	   ;; data is guaranteed to be not null

	   (let ((first-eval-cell-position
		  (first (first data)))
		 (last-eval-cell-position
		  (first (last1 data))))
	     (loop for (nil evaled-value stdout) in data 
		for cell in (chain all-cells (slice first-eval-cell-position)) do
		  (render-result cell evaled-value stdout))
	     (when (= command "evaled")  
	       (focus-to-next-cell (getprop all-cells last-eval-cell-position)))))
	  
	  ;; load file
	  ((= command "code")
	   ;; set values to cells
	   (chain (getprop (first-cell) 'editor) (get-doc) (set-value (getprop data 0)))
	   (loop for exp in (chain data (slice 1)) do
		(chain (getprop (make-cell) 'editor)
		       (get-doc) (set-value exp)))
	   ;; 
	   (get-it-focused (first-cell))
	   (eval-forward)
	   (change-title)
	   
	   
	   )
	  
	  ;; system error is simply displayed in the first cell
	  ((= command "systemError")
	   (blink-rename_span "SYSTEM ERROR")
	   (let ((fc (first-cell)))
	     (clear-result-area fc)
	     (setf (chain (getprop fc 'result-area)
			  |innerHTML|) data)
	     (get-it-focused fc)))

	  ((= command "systemMessage")
	   (cond ((= data "saved")
		  (setf ever-been-saved 1)
		  (blink-rename_span "Saved Successfully!!")
		  (change-title)
		  )
		 ((= data "interrupted")
		  (blink-rename_span "Interrupted!!"))))
	  
	  ;; won't ever happend
	  (t (alert "Unknown Command")))))

    (defun clear-result-area (cell)
      (setf (chain (getprop cell 'result-area) |innerHTML|)
	    ""))

    ;; don't worry about cell focusing here
    (defun render-result (cell evaled-value stdout)
      (let ((command (chain evaled-value command))
	    (data (chain evaled-value data)))
	(cond ((= command "drawChart")
	       (let ((div (getprop cell 'result-area)))
		 (clear-result-area cell)
		 ;; write standard output if exists
		 (unless (= stdout null)
		   (let ((p (chain document (create-element "p"))))
		     ;; "pre-wrap" may not be inherited
		     ;; check it out
		     (chain div (append-child p))
		     (setf (chain p |innerHTML|) stdout)))
		 (let ((inner-div (chain document (create-element "div"))))
		   (chain div (append-child inner-div))
		   ;; set size of the pane
		   (setf (@ inner-div style width)
			 (+ (chain data width (to-string)) "px"))
		   (setf (@ inner-div style height)
			 (+ (chain data height (to-string)) "px"))
		   (setf (@ inner-div title) (@ data title))
		   (draw-chart inner-div
			       (@ data series-list)
			       (@ data options)))))
	      

	      ((= command "text")
	       (let ((div (getprop cell 'result-area)))
		 (clear-result-area cell)
		 (setf (chain div |innerHTML|)
		       (if (= stdout "")
			   data
			   (chain "{0}<br>{1}" (format stdout data))))))
	      
	      ((= command "error")
	       (let ((div (getprop cell 'result-area)))
		 (clear-result-area cell)
		 (setf (chain div |innerHTML|) data))))))


    
    (defvar ws
      (new (|WebSocket|
	    (lisp
	     (format nil "ws://127.0.0.1:~A~A"
		     *web-socket-port* *ws-loc*)))))

    (setf (chain ws onopen)
	  (lambda ()
	    (make-cell)
	    (when (lisp notebook-filename)
	      (load-notebook-file (lisp notebook-filename)))
	    
	    ;; add key event ctrl-return to eval the focused cell	    
	    (chain ($ document)
		   (keydown (lambda (event)
			      (when (and (chain event ctrl-key)
					 (= (chain event which) 13))
				(eval-cell-and-go focused-cell "evalk")))))
	    (change-title)
	    (console.log "Openning connection to websocket")))    

    
    (setf (chain ws onmessage)
	  (lambda (event)
	    (handle-message (chain event data))))))


    

    

    

  

  




(defparameter *menubutton-size* "40px")


(defun notebook-page (&optional notebook-filename)
  (let ((untitled (string (gensym "UNTITLED"))))
    (with-html-output-to-string  (*standard-output* nil :prologue "<!DOCTYPE html>" :indent t)
      (:html
       :lang "en"
       (:head
	(:meta :charset "utf-8")
	;; register js and css files
	(loop for file in *js-css-files* do
	     (if (string= (pathname-type file) "css")
		 (htm (:link :rel "stylesheet" :type "text/css"
			     :href (shorten-file-name file)))
		 (htm (:script :type "text/javascript"
			       :src (shorten-file-name file)))))

	(:script :type "text/javascript" (str (js-for-notebook-page notebook-filename)))
	(:title :id "title" "YOMI"))

       ;; (setf (chain div-main style border-style) "solid")
       ;; (setf (chain div-main style border-radius) "0.5em")
       ;; (setf (chain div-main style border-width) "1px")

     
       (:body
      
	(:div :id "notebook-menu"
	      :style " background-color: #FFFFFF; overflow:hidden"
	      (:div :style "height:0px; overflow:hidden"
		    (:form  :action "/yomi" :method "post"
			    :enctype "multipart/form-data"
			    (:input
			     :type "file" :id "choose-notebook"
			     :name "yomifile"
			     :onchange
			     (ps (chain this form (submit))))
			    ))
	      ;; menu icons	    
	      (:div :style "display: inline-block"
		    (:input :type "image" :src "folder.png"
			    :width *menubutton-size* :height *menubutton-size*
			    :onclick "saveNotebook();chooseNotebook();")
		    (:input :type "image" :src "notebook.png"
			    :width *menubutton-size* :height *menubutton-size*
			    :onclick "openInNewTab(\"yomi\");")
		    "&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; "
		    (:input :type "image" :src "play.png"
			    :width *menubutton-size* :height *menubutton-size*
			    :onclick "evalCellAndGo(focusedCell)")
		    (:input :type "image" :src "fast_forward.png"
			    :width *menubutton-size* :height *menubutton-size*
			    :onclick "evalForward()")
		  
		    (:input :type "image" :src "stop.png"
			    :width *menubutton-size* :height *menubutton-size*
			    :onclick "interrupt() "

			    )
		  
		    "&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; "

		    (:div :style "display: inline-block"
			  (:input :type "image" :src "arrow_up.png"
				  :width *menubutton-size* :height *menubutton-size*
				  :onclick "moveUp()"
				  )
			  (:input :type "image" :src "arrow_down.png"
				  :width *menubutton-size* :height *menubutton-size*
				  :onclick "moveDown()"
				  )
			  (:input :type "image" :src "add.png"
				  :width *menubutton-size* :height *menubutton-size*
				  :onclick "addCell()"
				  )


			  (:input :type "image" :src "cut.png"
				  :width *menubutton-size* :height *menubutton-size*
				  :onclick "removeCell()"
				  )
			  (:input :type "image" :src "undo.png"
				  :width *menubutton-size* :height *menubutton-size*
				  :onclick "undo()"
				  )


			  )

		    )
	    
	      (:div :id "rename_div"
		    :style "display:inline-block; position:absolute; right: 0;margin-right:14px"
		    (:span :style "font-size:120%;color:gray;"
			   :onclick (ps (rename-input-show-up))
			   :id "rename_span"
			   (str untitled))
		    (:input :style "display:none;border-style:solid;
                                  border-radius:0.5em;border-color:gray;"
			    :type "text"
			    :id "rename_input"
			    :value untitled
			    :onkeydown
			    (ps
			      (if (= (@ event key-code) 13)
				  ;; 
				  (if (= (chain document
						(get-element-by-id "rename_input") ; feels stupid
						value (trim))
					 "")
				      (alert "Please Enter a Name")
				      (chain document (get-element-by-id "rename_button") (click))))))
		  
		    (:input :type "submit"
			    :style "display:none;"
			    :onclick "renameNotebook();"
			    :id "rename_button")
		  
		    (:input :type "image" :src "save.png"
			    :width *menubutton-size* :height *menubutton-size*
			    :onclick "saveNotebook()"))
	      )

	(:div :id "cellpad")
	)
     

       )
      )))


