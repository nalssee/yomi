;; 
;; Tidy up later, not now
;;

(in-package :yomi)


(defun js-for-notebook-page (notebook-filename)
  (ps
    (lisp *ps-lisp-library*)

    ;; parameters
    ;; I have no idea of the nameing conventions for Parenscript global variables.
    (defvar focused-cell nil)
    (defvar all-cells (array))

    ;; removed cell contents to recover
    ;; a list of cell position and editor value pairs
    (defvar removed-cell-contents (array))
    
    (defvar rename-span nil)
    (defvar rename-save nil)

    ;; Show system messages here
    
    (defvar cell-counter 0)

    ;; 0 : never been saved with the current file name
    ;; 1 : saved at least once
    (defvar ever-been-saved 0)

        
    (defun first (x) (getprop x 0))
    (defun rest (x) (chain x (slice 1)))
    (defun last1 (x)
      (getprop x (- (chain x length) 1)))
    

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
	  (notify "Processing" "orange")
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
      (let* ((cells-to-eval (chain all-cells
				   (slice (cell-position focused-cell))))
	     (first-cell-position (cell-position focused-cell)))
	(notify "Processing" "orange")
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
      (let ((divmain (getprop cell 'div-main))
	    (cla (getprop cell 'cell-loc-area)))
	(setf (chain divmain style border-color) "green")
	(setf (chain cla style color) "green")))
    
   
    (defun turn-off-border (cell)
      (when cell
	(let ((divmain (getprop cell 'div-main))
	      (cla (getprop cell 'cell-loc-area)))
	  (setf (chain divmain style border-color) "#FFFFFF")
	  (setf (chain cla style color) "gray")

	  )))
   
   
    ;; when the cell (actually div-outer) is clicked
    (defun get-it-focused (cell)
      (turn-off-border focused-cell)
      (setf focused-cell cell)
      (turn-on-border cell))

    

    ;; add a cell after the focused cell
    (defun add-cell ()
      (make-cell focused-cell))
    
      
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
			(- n 1))))
	  
	  (refresh-cell-loc)
	  ))
      
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
		(move-up)))
	  
	  (refresh-cell-loc)

	  )))
    


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
	  (refresh-cell-loc)
	  )))
    
    
    (defun move-down ()
      (when (not (last-cell-p focused-cell))
	(let* ((n (cell-position focused-cell))
	       (temp (getprop all-cells (1+ n))))
	  (insert-b (getprop temp 'div-outer)
		    (getprop focused-cell 'div-outer))
	  (setf (getprop all-cells (1+ n)) focused-cell)
	  (setf (getprop all-cells n) temp)
	  (auto-scroll)
	  (refresh-cell-loc)

	  )))
    

    ;; 
    (defun rename-input-show-up ()
      (let ((rename-input (chain document (get-element-by-id "rename_input"))))
	;; hide the file name span
	(setf (chain rename-span style display)  "none")
	;; show input and button
	(setf (chain rename-input style display) "inline")))


    
    ;; blink rename_span content for a second 
    (defun notify-blink (message color-string)
      ;; Maybe I shoud set a variable for rename span
      (let ((saved-span-content (chain rename-save |innerHTML|)))
	(unless (= (chain rename-span |innerHTML|) "Processing")
	  (setf (chain rename-span |innerHTML|) message)
	  (setf (chain rename-span style color) color-string)
	  (set-timeout (lambda ()
			 ;; revert it back
			 (setf (chain rename-span |innerHTML|) saved-span-content)
			 ;; gray is the default color
			 (setf (chain rename-span style color) "gray"))
		       1000)

	  )
	
	))

    (defun notify-blink-enforce (message color-string)
      ;; Maybe I shoud set a variable for rename span
      (let ((saved-span-content (chain rename-save |innerHTML|)))
	(setf (chain rename-span |innerHTML|) message)
	(setf (chain rename-span style color) color-string)
	(set-timeout (lambda ()
		       ;; revert it back
		       (setf (chain rename-span |innerHTML|) saved-span-content)
		       ;; gray is the default color
		       (setf (chain rename-span style color) "gray"))
		     1000)))



    

    (defun notify (message color-string)
      (setf (chain rename-span |innerHTML|) message)
      (setf (chain rename-span style color) color-string))


    (defun notify-revert ()
      (setf (chain rename-span |innerHTML|)
	    (chain rename-save |innerHTML|))
      (setf (chain rename-span style color) "GRAY"))

    
    

        
    
    (defun rename-notebook ()
      (let* ((rename-input  (chain document (get-element-by-id "rename_input")))
	     ;; trimmed rename-input value
	     (rename-input-value (chain rename-input value (trim))))

	(unless (= (chain rename-save |innerHTML|)
		   rename-input-value)
	  
	  (setf ever-been-saved 0)
	  (setf (chain rename-span |innerHTML|) rename-input-value)
	  (setf (chain rename-save |innerHTML|)
		(chain rename-span |innerHTML|)))
	
	(setf (chain rename-span style display)  "inline")
	(setf (chain rename-input style display) "none")

	

	))
    

    

    

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
	(let ((filename (chain rename-save |innerHTML|)))
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


    (defun pad (str max)
      (if (< (chain str length) max)
	  (pad (+ "0" str) max)
	  str))
    
    ;; not a very efficient way but
    ;; I don't want to bother.
    ;; computers are fast enough
    (defun refresh-cell-loc ()
      (loop for i from 1
	   for cell in all-cells do
	   (setf (chain (getprop cell 'cell-loc-area) |innerHTML|)
		 (pad (chain i (to-string)) 3))))

        

    ;; cell is an object with a div element and an editor in the element
    ;; if a sibling is given, make a cell after sibling
    ;; or make a cell as the first child of cellpad
    (defun make-cell (&optional sibling)
      (let ((div-outer (chain document (create-element "div")))
	    (div-main (chain document (create-element "div") ))
	    (textarea (chain document (create-element "textarea")))
	    (resultarea (chain document (create-element "div")))
	    ;; cell location
	    (cell-loc-area (chain document (create-element "div")))

	    )

	;; I think the following some lines must go to css file
	;; But I will just leave it be for this experiental phase.
	
	;; style
	;; 	(setf (chain resultarea style overflow) "auto")
	(setf (chain div-main style border-style) "solid")
	(setf (chain div-main style border-radius) "0.5em")
	;; invisible border
	(setf (chain div-main style border-color) "#FFFFFF")
	
	
	(setf (chain div-main style border-width) "1px")

	(setf (chain div-main style padding) "10px 10px 10px 10px")
	(setf (chain div-main style margin) "0 auto")
	(setf (chain div-main style width) "90%")
	(setf (chain div-main style display) "inline-block")

	(setf (chain cell-loc-area |innerHTML|) "000")
	(setf (chain cell-loc-area style color) "gray" )
	(setf (chain cell-loc-area style display) "inline-block" )
	(setf (chain cell-loc-area style padding) "10px" )
	
	;; resultarea style
	(setf (chain resultarea style white-space) "pre-wrap")
	(setf (chain resultarea style margin-left) "50px")
	
	;; append elements
	(chain div-main (append-child textarea))
	(chain div-main (append-child resultarea))

	
	(chain div-outer (append-child cell-loc-area))
	;; insert div-main into div-outer		
	(chain div-outer (append-child div-main))

	
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
		       key-map (lisp *keymap*)
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
			    editor editor

			    cell-loc-area cell-loc-area)))
	  
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

	  (refresh-cell-loc)
	  

	  ;; add event to div-outer
	  ;; when div-outer is clicked the cell is focused
	  (setf (chain div-outer onclick)
		(lambda () (get-it-focused cell)))
	  
	  ;; finally return the cell
	  cell)
	
	))

   
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
	(let ((filename (cutout-extension notebook-filename)))
	  (setf (chain rename-span |innerHTML|) filename)
	  (setf (chain rename-save |innerHTML|) filename)
	  (setf (chain document (get-element-by-id "rename_input") value)
		filename))))

    
    ;; chage title to notebook name
    (defun change-title ()
      (let ((title-element (chain document (get-element-by-id "title"))))
	(setf (chain title-element |innerHTML|)
	      (+ "YNB:"
		 (chain document (get-element-by-id "rename_input") value)))))
    

    ;; for future easy extension
    (defvar message-handling-function-set (create))

    
    (setf (getprop message-handling-function-set "evaled")
	  (lambda (data)
	    (notify-revert)
	    (let ((first-eval-cell-position (first (first data)))
		  (last-eval-cell-position  (first (last1 data))))
	      (loop for (nil evaled-value stdout) in data 
		 for cell in (chain all-cells (slice first-eval-cell-position)) do
		   (render-result (getprop cell 'result-area) evaled-value stdout))
	      (focus-to-next-cell (getprop all-cells last-eval-cell-position))
	      (auto-scroll))
	    


	    ))

    (setf (getprop message-handling-function-set "evaledk")
	  (lambda (data)
	    (notify-revert)
	    (let ((first-eval-cell-position (first (first data)))
		  (last-eval-cell-position  (first (last1 data))))
	      (loop for (nil evaled-value stdout) in data 
		 for cell in (chain all-cells (slice first-eval-cell-position)) do
		   (render-result (getprop cell 'result-area) evaled-value stdout))
	      ;; focus-to-next-cell is not invoked here

	      
	      )))
    
    (setf (getprop message-handling-function-set "code")
	  ;; set values to cells
	  (lambda (data)
	    (chain (getprop (first-cell) 'editor) (get-doc) (set-value (getprop data 0)))
	    
	    (loop for exp in (chain data (slice 1)) do
		 (chain (getprop (make-cell) 'editor)
			(get-doc) (set-value exp)))
	    	    
	    ;; <- todo
	    (get-it-focused (first-cell))
	    (eval-forward)
	    (change-title)
	    
	    ))


    
    (setf (getprop message-handling-function-set "systemError")
	  ;; set values to cells
	  (lambda (data)
	    (notify-blink "SYSTEM ERROR" "RED")
	    (let ((fc (first-cell)))
	      (clear-result-area fc)
	      (setf (chain (getprop fc 'result-area)
			   |innerHTML|) data)
	      (get-it-focused fc))))

    

    (setf (getprop message-handling-function-set "systemMessage")
	  ;; set values to cells
	  (lambda (data)
	    (cond ((= data "saved")
		   (setf ever-been-saved 1)
		   (notify-blink "Saved Successfully!!" "GREEN")
		   (change-title))
		  ((= data "interrupted")
		   (notify-blink-enforce "Interrupted!!" "dodgerblue")))))


    
    (defun handle-message (msg)
      (let* ((json-msg (chain +JSON+ (parse msg)))
	     (command (chain json-msg command))
	     (data (chain json-msg data)))

	((getprop message-handling-function-set command) data)))

    

    (defun clear-result-area (result-area)
      (setf (chain result-area |innerHTML|) ""))


    (defvar rendering-function-set (create))

    
    (setf (getprop rendering-function-set "drawChart")
	  (lambda (result-area data stdout)
	    (clear-result-area result-area)
	    ;; write standard output if exists	    
	    (unless (= stdout null)
	      (let ((p (chain document (create-element "p"))))
		;; "pre-wrap" may not be inherited
		;; check it out
		(chain result-area (append-child p))
		(setf (chain p |innerHTML|) stdout)))
	    (let ((inner-div (chain document (create-element "div"))))
	      

	      
	      (chain result-area (append-child inner-div))
	      ;; set size of the pane
	      (setf (@ inner-div style width)
		    (+ (chain data width (to-string)) "px"))
	      (setf (@ inner-div style height)
		    (+ (chain data height (to-string)) "px"))
	      (draw-chart inner-div
			  (@ data series-list)
			  (@ data options))
	      ;; write title if exists
	      (unless (= (@ data title) null)
		
		(setf (@ inner-div title) (@ data title))
		(let ((title-element (chain document (create-element "div"))))
		  (setf (@ title-element style width)
			(+ (chain data width (to-string)) "px"))
		  (chain result-area (append-child title-element))
		  (setf (chain title-element align) "center")
		  (setf (chain title-element |innerHTML|) (@ data title))))
	      )))


    (setf (getprop rendering-function-set "text")
	  (lambda (result-area data stdout)
	    (clear-result-area result-area)
	    (setf (chain result-area |innerHTML|)
		  (if (= stdout "")
		      data
		      (chain "{0}<br>{1}" (format stdout data))))))
    
    
    (setf (getprop rendering-function-set "code")
	  ;; stdout is ignored
	  (lambda (result-area data stdout)
	    ;; 
	    (if (last-cell-p focused-cell)
		(let ((first-cell focused-cell))
		  (loop for d1 in data do
		       (let* ((c1 (make-cell focused-cell)))
			 (chain (getprop c1 'editor) (get-doc) (set-value d1))))

		  ;; <- todo
		  ;; not perfect
		  (focus-to-next-cell first-cell)
		  (eval-forward)

		  )
		
		(progn
		  (setf (chain result-area |innerHTML|)
			"Unless this is the last cell and also focused, it is ignored.")))))
    

    (setf (getprop rendering-function-set "error")
	  (lambda (result-area data stdout)
	    (clear-result-area result-area)
	    (setf (chain result-area |innerHTML|) data)))


    ;; todo
    ;; Horizontal overflow scrolling failed,
    ;; try it later.
    (setf (getprop rendering-function-set "pack")
	  ;; stdout is ignored
	  (lambda (result-area data stdout)
	    (clear-result-area result-area)
	    (unless (= stdout "")
	      (let ((p (chain document (create-element "p"))))
		;; "pre-wrap" may not be inherited
		;; check it out
		(chain result-area (append-child p))
		(setf (chain p |innerHTML|) stdout)))
	    (loop for row in data do
		 (let ((div-row (chain document (create-element "div"))))
		   (chain result-area (append-child div-row))
		   (if (instanceof row |Array|)
		       (loop for c1 in row do
			    (let ((div1 (chain document (create-element "div"))))
			      (chain div-row (append-child div1))
			      (setf (chain div1 style display) "inline-block")
			      ;; stdout is ignored
			      (render-result div1 c1 "")))
		       (render-result div-row row ""))))))
    

    
    
    ;; don't worry about cell focusing here
    ;; resul-area is a div elementp
    (defun render-result (result-area evaled-value stdout)
      (let ((command (chain evaled-value command))
	    (data (chain evaled-value data)))
	((getprop rendering-function-set command) result-area data stdout)))


    
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


	    ;; add key event ctrl-s to save notebook
	    (chain ($ document)
		   (keydown (lambda (event)
			      (when (and (chain event ctrl-key)
					 (= (chain event which) 83))
				(save-notebook)))))
	    
	    
	    
	    (change-title)
	    (console.log "Openning connection to websocket")))    

    (setf (chain ws onmessage)
	  (lambda (event)
	    (handle-message (chain event data))))
    


    (setf (chain window onload)
	  (lambda (event)
	    ;; these elements are used often

	    (setf rename-span (chain document (get-element-by-id "rename_span")))
	    ;; rename span value includes spaces so must be trimmed first
	    ;; no idea why
	    (setf (chain rename-span |innerHTML|)
		  (chain rename-span |innerHTML| (trim)))
	    (setf rename-save (chain document (get-element-by-id "rename_save")))
	    (setf (chain rename-save |innerHTML|)
		  (chain rename-save |innerHTML| (trim)))

	    )))

  
  
  )


    


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
		    
		    (:span :style "display:none;"
			   :id "rename_save"
			   (str untitled))
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


