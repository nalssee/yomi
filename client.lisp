(in-package :yomi)

(defun js-for-notebook-page (notebook-filename)
  (ps
    (lisp *ps-lisp-library*)

    ;; ====================================================
    ;; Parameters
    ;; I have no idea of the nameing conventions for Parenscript global variables.
    ;; ====================================================

    ;; web socket
    (defvar ws nil)
    
    ;; A cell is a text input area and result area combined.
    (defvar focused-cell nil)
    (defvar all-cells (array))

    ;; removed cell contents to recover
    ;; a list of cell position and editor value pairs
    (defvar removed-cell-contents (array))

    ;; cellpad
    ;; cells are going to be appended in this element
    (defvar cell-pad nil)

    
    ;; Show system messages here    
    (defvar rename-span nil)
    (defvar rename-save nil)
    
    ;; How many cells are created
    (defvar cell-counter 0)

    ;; 0 : never been saved with the current file name
    ;; 1 : saved at least once
    (defvar ever-been-saved 0)

    ;; for future easy extension
    ;; Idea adopted from data directed programming in SICP
    (defvar message-handling-function-set (create))
    (defvar rendering-function-set (create))
    
    ;; =============================================
    ;; For CL compatibility
    ;; =============================================    
    (defun first (x) (getprop x 0))
    (defun rest  (x) (chain x (slice 1)))
    (defun last1 (x)
      (getprop x (- (chain x length) 1)))

    (defun position (x xs)
      (let ((result nil))
	(loop for i from 0
	   for xi in xs do
	     (when (= x xi)
	       (setf result i)
	       (return)))
	result))
    
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

    

    
    ;; ========================================================
    ;; User Interface
    ;; Button actions from web browser
    ;; =========================================================


    (defun open-in-new-tab (url)
      (setf win (window.open url "_blank"))
      (chain win (focus)))
    
    (defun choose-notebook ()
      (chain document (get-element-by-id "choose-notebook") (click)))

    ;; ws is the name of websocket

    ;; command can be either "eval" or "evalk"
    ;; "evalk" means evaluation by keyboard short cut (Ctrl-Enter)
    ;; See for yourself what's the difference
    (defun eval-cell-and-go (cell &optional (command "eval"))
      (let ((cell-content (chain (getprop cell 'editor) (get-value)))
	    (cell-no (cell-position cell)))
	;; It is meaninless to evaluate empty cell
	;; Even if it is the empty cell if it's not the last cell
	;; it is evaluated for convenience
	(when (not (and (last-cell-p cell)
			(= cell-content "")))
	  (notify "Processing" "orange")
	  ;; Message is sent as a JSON string
	  (chain ws (send (chain +JSON+ (stringify
					 (make-message
					  command
					  ;; Even if we are evaluating only one cell
					  ;; it is sent as a list to avoid in accordance
					  ;; with eval-forward.
					  (array
					   ;; List may not be the best
					   ;; data structure for this job
					   ;; I just want to simplify it though.
					   (array cell-no cell-content))))))))))
    
    (defun interrupt ()
      (chain ws (send (chain +JSON+ (stringify
				     (make-message
				      "interrupt"
				      ""))))))

   
    ;; Eval all cells from the focused cell
    ;; Caution: Not all cells, all cells from the focused cell.
    (defun eval-forward ()
      (let* ((cells-to-eval (chain all-cells
				   (slice (cell-position focused-cell))))
	     (first-cell-position (cell-position focused-cell)))
	(notify "Processing" "orange")
	(chain
	 ws
	 (send (chain +JSON+ (stringify
			      (make-message
			       "eval"
			       (loop for c in cells-to-eval
				  for i from first-cell-position collect
				    (array i (chain (getprop c 'editor) (get-value)))))))))))

    
    (defun move-up ()
      (let ((n (cell-position focused-cell)))
	;; only when focused-cell is not the first cell
	(when (> n 0)
	  (let ((previous (getprop all-cells (1- n))))
	    ;; insert before
	    ;; this is a bit different from insertBefore, side effect only
	    (insert-b (getprop focused-cell 'div-outer)
		      (getprop previous 'div-outer))
	    ;; all-cells variable must be handled correctly
	    ;; swap them
	    (setf (getprop all-cells (1- n)) focused-cell)	    
	    (setf (getprop all-cells n) previous)
	    ;; the following should be obvious
	    (auto-scroll)
	    (refresh-cell-loc)))))
    
    
    (defun move-down ()
      ;; only when the focused cell is not the last one
      (when (not (last-cell-p focused-cell))
	(let* ((n (cell-position focused-cell))
	       ;; next cell
	       (next (getprop all-cells (1+ n))))
	  (insert-b (getprop next 'div-outer)
		    (getprop focused-cell 'div-outer))
	  
	  (setf (getprop all-cells (1+ n)) focused-cell)
	  (setf (getprop all-cells n) next)
	  (auto-scroll)
	  (refresh-cell-loc))))


    ;; add a cell after the focused cell
    (defun add-cell ()
      (make-cell focused-cell))
    
    ;; Remove a focused-cell and focus goes up
    ;; if there's only one cell do nothing
    (defun remove-cell ()
      (when (> (chain all-cells length) 1)
	(let ((div-outer (getprop focused-cell 'div-outer))
	      (n (cell-position focused-cell)))
	  ;; Save the removed cell value in removed-cell-contents
	  ;; for future recover
	  ;; only editor value is saved
	  ;; Content in result area is simple thrown away
	  (chain removed-cell-contents
		 (push (array n		; cell position at removal
			      (chain
			       (getprop (getprop all-cells n) 'editor) (get-value)))))
	  ;; cut it out
	  (chain all-cells (splice n 1))
	  (chain div-outer (remove))
	  (get-it-focused
	   (getprop all-cells (chain |Math| (max 0 (- n 1)))))
	  
	  (refresh-cell-loc))))
    

    ;; Recover the last removed cell
    (defun undo ()
      (when (> (chain removed-cell-contents length) 0)
	(let* ((to-recover (chain removed-cell-contents (pop)))
	       ;; cell postion at its removal.
	       ;; Recovered cell will be there.
	       (n (getprop to-recover 0))
	       (content (getprop to-recover 1)))
	  (if (= n (chain all-cells length))
	      ;; last cell is removed
	      (chain (getprop (make-cell) 'editor) (get-doc) (set-value content))
	      (let ((cell (make-cell (getprop all-cells n))))
		(chain (getprop cell 'editor) (get-doc) (set-value content))
		(move-up)))
	  (refresh-cell-loc))))
    

    ;; 
    (defun rename-input-show-up ()
      (let ((rename-input (chain document (get-element-by-id "rename_input"))))
	;; hide the file name span
	(setf (chain rename-span style display)  "none")
	;; show input and button
	(setf (chain rename-input style display) "inline")))

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
	(setf (chain rename-input style display) "none")))

    (defun save-notebook ()
      (unless (empty-notebook-p)
	(let ((filename (chain rename-save |innerHTML|)))
	  (chain
	   ws
	   (send (chain +JSON+
			(stringify
			 (make-message
			  (if (= ever-been-saved 0)
			      "saveFileWithCaution"
			      "saveFile")
			  ;; first element of data list is a filename to save
			  (append (list filename)
				  (loop for cell in all-cells collect
				       (chain (getprop cell 'editor) (get-value))))))))))))
    
    
    (defun load-notebook-file (notebook-filename)
      (setf ever-been-saved 1)
      (when notebook-filename
	(chain ws (send (chain +JSON+ (stringify
				       (make-message
					"loadFile"
					notebook-filename)))))
	(let ((filename (cutout-extension notebook-filename)))
	  (setf (chain rename-span |innerHTML|) filename)
	  (setf (chain rename-save |innerHTML|) filename)
	  (setf (chain document (get-element-by-id "rename_input") value)
		filename))))

    
    ;; =========================================================
    ;; While buttons send messages to the server through web socket
    ;; 'handle-message' handles messages from the server
    ;; =========================================================

    ;; handle-message pairs with handle-message defined in server.lisp
    (defun handle-message (msg)
      (let* ((json-msg (chain +JSON+ (parse msg)))
	     ;; Message from the server is composed of command and data
	     (command (chain json-msg command))
	     (data (chain json-msg data)))
	((getprop message-handling-function-set command) data)))
    
    ;; Register functions for message handling
    
    (setf (getprop message-handling-function-set "evaled")
	  ;; data is a list of evaled-result
	  (lambda (data)
	    ;; rename_span message reverted back to notebook name
	    (notify-revert)
	    (let ((first-eval-cell-position (chain (first data) cellno))
		  (last-eval-cell-position  (chain (last1 data) cellno)))
	      (loop for d1 in data
		 for cell in (chain all-cells (slice first-eval-cell-position)) do
		   (render-result (getprop cell 'result-area) d1))
	      (focus-to-next-cell (getprop all-cells last-eval-cell-position))
	      (auto-scroll))))
    
    ;; evaluation message is send by keyboard shortcut (Ctrl-Enter)
    ;; the following function handles the message from the action
    ;; almost the same as the above except the last part
    (setf (getprop message-handling-function-set "evaledk")
	  (lambda (data)
	    (notify-revert)
	    (let ((first-eval-cell-position (chain (first data) cellno)))
	      (loop for d1 in data 
		 for cell in (chain all-cells (slice first-eval-cell-position)) do
		   (render-result (getprop cell 'result-area) d1))
	      ;; focus-to-next-cell is not invoked here
	      )))
    
    ;; Notebook file loading 
    (setf (getprop message-handling-function-set "code")
	  ;; set values to cells
	  (lambda (data)
	    ;; first cell is made at the page loading
	    ;; hence no need for make-cell
	    (chain (getprop (first-cell) 'editor) (get-doc) (set-value (getprop data 0)))
	    (loop for exp in (chain data (slice 1)) do
		 (chain (getprop (make-cell) 'editor)
			(get-doc) (set-value exp)))
	    ;; focus back to the first cell and eval-forward
	    (get-it-focused (first-cell))
	    (eval-forward)
	    (change-title)))


    ;; System error handling
    (setf (getprop message-handling-function-set "systemError")
	  ;; set values to cells
	  (lambda (data)
	    (notify-blink "SYSTEM ERROR" "RED")
	    (let ((fc (first-cell)))
	      (clear-result-area fc)
	      (setf (chain (getprop fc 'result-area) |innerHTML|) data)
	      (get-it-focused fc))))
    
    ;; System messages like "saved" or "interrupted" 
    (setf (getprop message-handling-function-set "systemMessage")
	  (lambda (data)
	    (cond ((= data "saved")
		   ;; This message is arrived means that
		   ;; the file is saved well.
		   (setf ever-been-saved 1)
		   (notify-blink "Saved Successfully!!" "GREEN")
		   (change-title))
		  ((= data "interrupted")
		   (notify-blink-enforce "Interrupted!!" "dodgerblue")))))
    


    ;; =========================================================
    ;; Now you need to render results in the result area
    ;; render-result pairs with "build-message-to-send' in server.lisp
    ;; =========================================================
    
    ;; don't worry about cell focusing here
    ;; resul-area is a div elementp
    (defun render-result (result-area evaled-result)
      ((getprop rendering-function-set (@ evaled-result type))
       result-area
       (@ evaled-result value) (@ evaled-result stdout)))
    
    
    ;; Register result rendering functions
    
    ;; Text rendering
    (setf (getprop rendering-function-set "text")
	  (lambda (result-area value stdout)
	    (clear-result-area result-area)
	    (setf (chain result-area |innerHTML|)
		  (if (= stdout "")
		      value
		      (chain "{0}<br>{1}" (format stdout value))))))
    

    ;; Draw chart using "flot"
    (setf (getprop rendering-function-set "chart")
	  (lambda (result-area value stdout)
	    (clear-result-area result-area)
	    ;; write standard output if exists	
	    (unless (= stdout null)
	      (let ((p (chain document (create-element "p"))))
		;; "pre-wrap" is inherited
		;; no need for style setup
		(chain result-area (append-child p))
		(setf (chain p |innerHTML|) stdout)))
	    
	    (let ((inner-div (chain document (create-element "div"))))
	      (chain result-area (append-child inner-div))
	      ;; set size of the pane
	      (setf (@ inner-div style width)
		    ;; to-string is not a must.
		    (+ (chain value width (to-string)) "px"))
	      (setf (@ inner-div style height)
		    (+ (chain value height (to-string)) "px"))
	      
	      (draw-chart inner-div (@ value series-list)  (@ value options))
	      ;; write title if exists
	      (unless (= (@ value title) null)
		(setf (@ inner-div title) (@ value title))
		(let ((title-element (chain document (create-element "div"))))
		  (setf (@ title-element style width)
			(+ (chain value width (to-string)) "px"))
		  (chain result-area (append-child title-element))
		  (setf (chain title-element align) "center")
		  (setf (chain title-element |innerHTML|) (@ value title)))))))

    

    ;; code rendering is different from notebook file loading
    ;; You can type in message in text area and the server sends a message
    ;; to generate new cells
    ;; refer to "defcode" macro in summary.lisp
    (setf (getprop rendering-function-set "code")
	  ;; stdout is ignored
	  (lambda (result-area value stdout)
	    (if (last-cell-p focused-cell)
		(let ((first-cell focused-cell))
		  (loop for v1 in value do
		       (let ((c1 (make-cell focused-cell)))
			 (chain (getprop c1 'editor) (get-doc) (set-value v1))))
		  (focus-to-next-cell first-cell)
		  (eval-forward))
		(setf (chain result-area |innerHTML|)
		      (chain "{0}<br>&nbsp; &nbsp; {1}"
			     (format "Unless this is the focused last cell,"
				     "cell generation expression is ignored."))))))
    
    
    ;; Render error message
    (setf (getprop rendering-function-set "error")
	  (lambda (result-area value stdout)
	    (clear-result-area result-area)
	    (setf (chain result-area |innerHTML|) value)))


    ;; Sometimes you may want to render multiple results in a single
    ;; result area, especially for plots.
    ;; You may want to pack up multple plots
    
    ;; todo
    ;; Horizontal overflow scrolling failed,
    ;; try it later.
    ;; I haven't figured out yet which one is better.
    (setf (getprop rendering-function-set "packv")
	  (lambda (result-area value stdout)
	    (clear-result-area result-area)
	    ;; render standard output first
	    (unless (= stdout "")
	      (let ((p (chain document (create-element "p"))))
		(chain result-area (append-child p))
		(setf (chain p |innerHTML|) stdout)))
	    (loop for v1 in value do
		 (let ((div (chain document (create-element "div"))))
		   (chain result-area (append-child div))
		   (render-result div v1)))))
    

    (setf (getprop rendering-function-set "packh")
	  (lambda (result-area value stdout)
	    (clear-result-area result-area)
	    ;; render standard output first
	    (unless (= stdout "")
	      (let ((p (chain document (create-element "p"))))
		(chain result-area (append-child p))
		(setf (chain p |innerHTML|) stdout)))
	    (loop for h1 in value do
		 (let ((div (chain document (create-element "div"))))
		   (chain result-area (append-child div))
		   ;; <--
		   (setf (chain div style display) "inline-block")
		   (render-result div h1)))))  

    
    

    ;; ====================================================
    ;; Make cell
    ;; ====================================================
    ;; cell is an object with a div element, an editor and some others.
    ;; If a sibling (another cell) is given, make a cell after the sibling,
    ;; otherwise append a cell as a child of "cell-pad" element
    
    ;; make-cell is a bit long, but most of the lines are just
    ;; simple sequential chores
    (defun make-cell (&optional sibling)
      (let ((div-outer (chain document (create-element "div")))
	    (div-main (chain document (create-element "div") ))
	    (textarea (chain document (create-element "textarea")))
	    (resultarea (chain document (create-element "div")))
	    ;; cell location
	    ;; at the left of the textarea cell number is shown for convenience
	    (cell-loc-area (chain document (create-element "div"))))

	;; I think the following some lines must go to css file
	;; But I will just leave it be for this experiental phase.
	
	(setf (chain div-main class-name) "cell-div-main")
	(setf (chain cell-loc-area class-name) "cell-loc-area")
	;; place holder
	(setf (chain cell-loc-area |innerHTML|) "000")
	(setf (chain resultarea class-name) "result-area")
	
	;; Append elements
	(chain div-main (append-child textarea))
	(chain div-main (append-child resultarea))

	(chain div-outer (append-child cell-loc-area))
	;; insert div-main into div-outer		
	(chain div-outer (append-child div-main))

	;; You must first attach a div element to a document before it wears CodeMirror.
	(if (= sibling undefined)
	    (chain cell-pad (append-child div-outer))
	    (insert-a div-outer (getprop sibling 'div-outer)))

	;; 
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
	
	;; Disable ctrl-enter in the editor
	;; it is going to be used as cell evaluation
	;; if you don't include the following expression
	;; the editor will insert a newline for ctrl-enter
	(chain editor (set-option
		       "extraKeys"
		       (create "Ctrl-Enter"
			       (lambda (cm)))))

	;; Set id to div-outer for later use
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
	  (auto-scroll)
	  
	  ;; all-cells variable fixed
	  (if (= sibling undefined)
	      (chain all-cells (push cell))
	      (chain all-cells (splice (1+ (cell-position sibling)) 0 cell)))
	  
	  ;; cell numbering must be changed
	  (refresh-cell-loc)
	  
	  ;; add event to div-outer
	  ;; when div-outer is clicked the cell is focused
	  (setf (chain div-outer onclick)
		(lambda () (get-it-focused cell)))
	  
	  ;; finally return the cell
	  cell)))


    ;; ====================================================
    ;; misc.
    ;; ====================================================
    
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
	  (setf (chain cla style color) "gray"))))
    
    ;; when the cell (actually div-outer) is clicked
    (defun get-it-focused (cell)
      (turn-off-border focused-cell)
      (setf focused-cell cell)
      (turn-on-border cell))

    
    ;; scroll to focused element to show up
    (defun auto-scroll ()
      (chain (getprop focused-cell 'div-outer) (scroll-into-view)))

    ;; Blink rename_span content for a second
    ;; unless the server is processing the request
    (defun notify-blink (message color-string)
      (let ((saved-span-content (chain rename-save |innerHTML|)))
	;;
	(unless (= (chain rename-span |innerHTML|) "Processing")
	  (setf (chain rename-span |innerHTML|) message)
	  (setf (chain rename-span style color) color-string)
	  (set-timeout (lambda ()
			 ;; revert it back
			 (setf (chain rename-span |innerHTML|) saved-span-content)
			 ;; gray is the default color
			 (setf (chain rename-span style color) "gray"))
		       ;; 1 sec
		       1000))))
    
    ;; Blink rename_span no matter what
    ;; used for "interrupt"
    (defun notify-blink-enforce (message color-string)
      (let ((saved-span-content (chain rename-save |innerHTML|)))
	(setf (chain rename-span |innerHTML|) message)
	(setf (chain rename-span style color) color-string)
	(set-timeout (lambda ()
		       ;; revert it back
		       (setf (chain rename-span |innerHTML|) saved-span-content)
		       ;; gray is the default color
		       (setf (chain rename-span style color) "GRAY"))
		     1000)))

    ;; Change rename_span message
    (defun notify (message color-string)
      (setf (chain rename-span |innerHTML|) message)
      (setf (chain rename-span style color) color-string))
    

    ;; Revert rename_span content to the one on rename_save,
    ;; which is a file(notebook) name
    (defun notify-revert ()
      (setf (chain rename-span |innerHTML|)
	    (chain rename-save |innerHTML|))
      (setf (chain rename-span style color) "GRAY"))

    ;; not a very efficient way but
    ;; I don't want to bother.
    ;; computers are fast enough
    (defun refresh-cell-loc ()
      (loop for i from 1
	 for cell in all-cells do
	   (setf (chain (getprop cell 'cell-loc-area) |innerHTML|)
		 (pad (chain i (to-string)) 3))))



    
    ;; ====================================================
    ;; Helpers
    ;; ====================================================

    ;; (defun draw-chart (place data options)
    ;;   (chain ($ document)
    ;; 	     (ready (lambda ()
    ;; 		      (chain $ (plot place data options))))))
    
    (defun draw-chart (place data options)
      (chain $ (plot place data options)))
    
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
    
    (defun first-cell () (first all-cells))    
    (defun last-cell () (last1 all-cells))

    (defun last-cell-p (cell)
      (= (getprop (last-cell) 'no)
	 (getprop cell 'no)))

    (defun empty-cell-p (cell)
      (= (chain (getprop cell 'editor) (get-value)) ""))
    

    ;; returns an integer
    (defun cell-position (cell)
      (position (getprop cell 'no)
		(mapcar (lambda (c) (getprop c 'no)) all-cells)))
    
    ;; make a message to send to the server throught web socket
    (defun make-message (command data)
      (create command command
	      data data))
    
    (defun empty-notebook-p ()
      (and (= (chain all-cells length) 1)
	   (= ""
	      (chain (getprop (first-cell) 'editor) (get-value)))))
    
    ;; put '0' at front for example
    ;; (pad '5' 3) => '005'
    (defun pad (str max)
      (if (< (chain str length) max)
	  (pad (+ "0" str) max)
	  str))

    ;; 
    (defun cutout-extension (filename)
      (let ((splited  (chain filename (split "."))))
	(if (= (chain splited length) 1)
	    (getprop splited 0)
	    (chain splited (splice 0 (- (chain splited length) 1)) (join ".")))))
    
    
    ;; chage title to notebook name
    (defun change-title ()
      (let ((title-element (chain document (get-element-by-id "title"))))
	(setf (chain title-element |innerHTML|)
	      (+ "YNB "
		 (chain document (get-element-by-id "rename_input") value)))))

    (defun clear-result-area (result-area)
      (setf (chain result-area |innerHTML|) ""))
    

    ;; ===========================================
    ;; On page loading 
    ;; ===========================================
    (setf ws
	  (new (|WebSocket|
		(lisp
		 (format nil "ws://127.0.0.1:~A~A"
			 *web-socket-port* *ws-loc*)))))
    
    (setf (chain ws onopen)
	  (lambda ()
	    ;; cell-pad
	    (setf cell-pad (chain document (get-element-by-id "cellpad")))
	    (make-cell)
	    ;; when notebook-file is given as an argument
	    (when (lisp notebook-filename)
	      (load-notebook-file (lisp notebook-filename)))
	    
	    ;; add key event ctrl-return to eval the focused ce
	    (chain ($ document)
		   (keydown (lambda (event)
			      (when (and (chain event ctrl-key)
					 ;; 13 represents "Enter key"
					 (= (chain event which) 13))
				(eval-cell-and-go focused-cell "evalk")))))

	    ;; add key event ctrl-s to save notebook
	    (chain ($ document)
		   (keydown (lambda (event)
			      (when (and (chain event ctrl-key)
					 ;; 83 represents "s"
					 (= (chain event which) 83))
				(save-notebook)))))
	    
	    ;; Change title value to file name
	    (change-title)
	    (console.log "Openning connection to websocket")))
    
    ;; When messages reached to the client through web socket
    (setf (chain ws onmessage)
	  (lambda (event)
	    (handle-message (chain event data))))


    
    (defun trim-spaces (element)
      (setf (chain element |innerHTML|)
	    (chain element |innerHTML| (trim))))

    ;; When the web page is loaded
    (setf (chain window onload)
	  (lambda (event)
	    ;; rename-span is for broadcasting, sort of.
	    ;; Hence used often
	    (setf rename-span (chain document (get-element-by-id "rename_span")))
	    ;; rename span value includes spaces so must be trimmed first
	    (trim-spaces rename-span)
	    ;; notebook file name is saved here
	    (setf rename-save (chain document (get-element-by-id "rename_save")))
	    (trim-spaces rename-save))))
  )

    

;; page html

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
	     (cond
	       ((string= (pathname-type file) "css")
		(htm (:link :rel "stylesheet" :type "text/css"
			    :href (shorten-file-name file))))
	       (t (htm (:script :type "text/javascript"
				:src (shorten-file-name file))))))
	
	(:script :type "text/javascript" :src "https://www.google.com/jsapi")
	(:script :type "text/javascript" (str (js-for-notebook-page notebook-filename)))
	(:title :id "title" "YOMI"))

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
			    :onclick (ps (save-notebook) (choose-notebook)))
		    (:input :type "image" :src "notebook.png"
			    :width *menubutton-size* :height *menubutton-size*
			    :onclick (ps (open-in-new-tab "yomi")))
		    "&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; "
		    (:input :type "image" :src "play.png"
			    :width *menubutton-size* :height *menubutton-size*
			    :onclick (ps (eval-cell-and-go focused-cell)))
		    (:input :type "image" :src "fast_forward.png"
			    :width *menubutton-size* :height *menubutton-size*
			    :onclick (ps (eval-forward)))
		  
		    (:input :type "image" :src "stop.png"
			    :width *menubutton-size* :height *menubutton-size*
			    :onclick (ps (interrupt)))
		  
		    "&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; "

		    (:div :style "display: inline-block"
			  (:input :type "image" :src "arrow_up.png"
				  :width *menubutton-size* :height *menubutton-size*
				  :onclick (ps (move-up))
				  )
			  (:input :type "image" :src "arrow_down.png"
				  :width *menubutton-size* :height *menubutton-size*
				  :onclick (ps (move-down)))
			  (:input :type "image" :src "add.png"
				  :width *menubutton-size* :height *menubutton-size*
				  :onclick (ps (add-cell)))


			  (:input :type "image" :src "cut.png"
				  :width *menubutton-size* :height *menubutton-size*
				  :onclick (ps (remove-cell)))
			  (:input :type "image" :src "undo.png"
				  :width *menubutton-size* :height *menubutton-size*
				  :onclick (ps (undo)))))
	    
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
			    :onclick (ps (rename-notebook)) 
			    :id "rename_button")
		  
		    (:input :type "image" :src "save.png"
			    :width *menubutton-size* :height *menubutton-size*
			    :onclick (ps (save-notebook))))
	      )

	(:div :id "cellpad"))))))






