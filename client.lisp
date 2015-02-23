(in-package :yomi)

;; ======================================================
;; Client -> Server
;; A message is transferd as a list of JSON strings
;; (A list of JSON strings means once you decode it using cl-json
;; it becomes a list of strings)
;; ======================================================

;; The first element is a command (a dispatching message)
;; (It is of course possible to have multiple command strings but not yet)
;; and the rest is data related to the command

;; 1. command (the first element of the list) : eval (evalkey, evalon)
;     evalkey (evaluation by keyboard action, ctrl + enter)
;;    evalon (evalate the follow-up cells as well till the end.
;;            Even for evalon, you just send one cell content. not them all
;;            the reason is not very simple)
;;    data (the rest):  (cell-number cell-content)

;; 2. command: interrupt (interrupt)
;     As for interrupt, a message is a single element list

;; 3. command: loadFile
;;    data:    (filename) (ex, "foo.yomi"), length = 1

;;  When the file already exists in the working directory, sends an error message.
;;  Of course it is safe to save a file multiple times,
;;  if it's a loaded file or newly created file
;; 4. command: saveFile (or saveFileEnforce)
;;    data:    (filename cell-content-1 cell-content-2 ...)

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
    ;; cell-pad-top-position is used in auto-scroll
    (defvar cell-pad-top-position nil)

    ;; Show system messages here    
    (defvar rename-span nil)
    (defvar notice nil)
        
    ;; How many cells are created
    (defvar cell-counter 0)
    ;; 0 : never been saved with the current file name
    ;; 1 : saved at least once
    (defvar ever-been-saved 0)

    (defvar current-package nil)

    ;; for future easy extension
    ;; Idea adopted from data directed programming in SICP
    (defvar message-handling-function-set (create))
    (defvar rendering-function-set (create))
    
    ;; =============================================
    ;; For CL compatibility, far from perfection even for those defined
    ;; =============================================    
    (defun first (x) (getprop x 0))
    (defun rest  (x) (chain x (slice 1)))
    (defun last1 (x)
      (getprop x (- (chain x length) 1)))
    
    (defun position (x xs)
      (let ((val (chain xs (index-of x))))
	(unless (< val 0) val)))
    
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
    ;; Most of the actions are prohibited while on processing,
    ;; You need to interrupt it first.
    
    (defun open-in-new-tab (url)
      (setf win (window.open url "_blank"))
      (chain win (focus)))
    
    (defun choose-notebook ()
      (unless (processing-p)
	(chain document (get-element-by-id "choose-notebook") (click))))

    ;; command can be either "eval", "evalkey" or "evalon"
    (defun eval-cell (cell &optional (command "eval"))
      (unless (processing-p)
	(let ((cell-content (chain (getprop cell 'editor) (get-value)))
	      (cell-no (getprop cell 'no)))
	  ;; It is meaninless to evaluate empty cell
	  ;; Even if it is the empty cell if it's not the last cell
	  ;; it is evaluated for convenince
	  (unless (and (last-cell-p cell)
		       (= (chain cell-content (trim)) ""))
	    (notify "PROCESSING" "orange")
	    (send-message (array command cell-no cell-content))))))
    
    (defun interrupt ()
      (send-message (array "interrupt")))
        
    (defun move-up ()
      (unless (processing-p)
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
	      (refresh-cell-loc))))))
    
    (defun move-down ()
      (unless (processing-p)
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
	    (refresh-cell-loc)))))

    ;; add a cell after the focused cell
    (defun add-cell ()
      (unless (processing-p)
	(make-cell focused-cell)
	(notify "" "")))
    
    ;; Remove a focused-cell and focus goes up
    ;; if there's only one cell do nothing
    (defun remove-cell ()
      (unless (processing-p)
	(when (> (chain all-cells length) 1)
	  (let ((div-outer (getprop focused-cell 'div-outer))
		(n (cell-position focused-cell)))
	    ;; Save the removed cell value in removed-cell-contents
	    ;; for future recover
	    ;; only editor value is saved
	    ;; Content in result area is simple thrown away
	    (chain removed-cell-contents
		   (push (array n	; cell position at removal
				(chain
				 (getprop (getprop all-cells n) 'editor) (get-value)))))
	    ;; cut it out
	    (chain all-cells (splice n 1))
	    (chain div-outer (remove))
	    (get-it-focused
	     (getprop all-cells (chain |Math| (max 0 (- n 1)))))
	    (refresh-cell-loc)))))
    
    ;; Recover the last removed cell
    (defun undo ()
      (unless (processing-p)
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
	    (refresh-cell-loc)))))
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
	(unless (= (chain rename-span |innerHTML|) rename-input-value)
	  (setf ever-been-saved 0)
	  (setf (chain rename-span |innerHTML|) rename-input-value)
	  (setf (chain rename-span style color)  "Gray"))
	(setf (chain rename-span style display)  "inline")
	(setf (chain rename-input style display) "none")))

    (defun save-notebook ()
      (unless (or (empty-notebook-p) (processing-p))
	(send-message
	 (append 
	  (array (if (= ever-been-saved 0)  "saveFile" "saveFileEnforce")
		 ;; file name to save
		 (chain rename-span |innerHTML|))
	  ;; cell contents in all-cells
	  (loop for cell in all-cells collect
	       (chain (getprop cell 'editor) (get-value)))))))
    
    (defun load-notebook-file (notebook-filename)
      (setf ever-been-saved 1)
      (send-message (array "loadFile" notebook-filename)))
    
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
	  ;; data is an evaled-result
	  (lambda (data)
	    ;; find a cell to render
	    (let ((cell (find-cell (@ data cellno))))
	      ;; result area can be broken into pieces (vpack, hpack)
	      ;; and sometimes you may want to modify cell elements
	      ;; other than its result-area
	      (render-result cell (getprop cell 'result-area) data)
	      ;; Just in case a user clicks other cells while processing
	      (if (= "code" (@ data type))
		  (focus-to-next-cell focused-cell)
		  (focus-to-next-cell cell))
	      ;; "PROCESSING" message out
	      (notify "" ""))))
    
    ;; evaluation message is send by keyboard shortcut (Ctrl-Enter)
    (setf (getprop message-handling-function-set "evaledkey")
	  (lambda (data)
	    (let ((cell (find-cell (@ data cellno))))
	      (render-result cell (getprop cell 'result-area) data)
	      ;; focusing stays, so no need to do anything unless
	      ;; the rendering includes a cell generation
	      (get-it-focused cell)
	      (notify "" ""))))
    
    ;; Evaluation goes on till the end
    ;; Looks simple and dull but this is written very carefully,
    ;; YOU MUST BE VERY CAUTIOUS IF YOU EVER WANT TO FIX THIS!!!
    (setf (getprop message-handling-function-set "evaledon")
	  (lambda (data)
	    (let ((cell (find-cell (@ data cellno))))
	      (cond ((and (last-cell-p cell) (not (= (@ data type) "code")))
		     ;; end case
		     (render-result cell (getprop cell 'result-area) data)
		     (focus-to-next-cell cell)
		     (notify "" ""))
		    (t (cond ((= (@ data type) "code")
			      (render-result cell (getprop cell 'result-area) data)
			      ;; Thawing
			      (notify "" "")
			      (eval-cell cell "evalon"))
			     (t
			      (render-result cell (getprop cell 'result-area) data)
			      (focus-to-next-cell cell)
			      (notify "" "")
			      (eval-cell focused-cell "evalon"))))))))
    
    ;; Notebook file loading 
    (setf (getprop message-handling-function-set "code")
	  ;; set values to cells
	  (lambda (data)
	    (let ((filename (cutout-extension (first data)))
		  (data (getprop data 1)))
	      ;; first cell is made at the page loading
	      ;; hence no need for make-cell
	      (chain (getprop (first-cell) 'editor) (get-doc) (set-value (first data)))
	      (loop for exp in (rest data) do
		   (let ((cell (make-cell)))
		     (chain (getprop cell 'editor)
			    (get-doc) (set-value exp))))
	      (setf (chain rename-span |innerHTML|) filename)
	      (setf (chain rename-span style color) "Green")
	      (setf (chain document (get-element-by-id "rename_input") value)
	      	    filename)
	      (change-title))))
    
    ;; System error handling
    (setf (getprop message-handling-function-set "systemError")
	  ;; set values to cells
	  (lambda (data)
	    (let ((fc (first-cell)))
	      (setf (chain (getprop fc 'result-area) |innerHTML|)
		    (chain "<font color='red'>{0}</font>"  (format data)))
	      ;; hide editor
	      (setf (chain (getprop fc 'editor)
			   (get-wrapper-element) style display) "none")
	      ;; hide cell loc area
	      (setf (chain (getprop fc 'cell-loc-area) style visibility) "hidden")
	      (get-it-focused fc)
	      (notify "SYSTEM ERROR" "RED"))))
    
    ;; System messages like "saved" or "interrupted" 
    (setf (getprop message-handling-function-set "systemMessage")
	  (lambda (data)
	    (cond ((= data "saved")
		   ;; This message is arrived means that
		   ;; the file is saved well.
		   (setf ever-been-saved 1)
		   (setf (chain rename-span style color) "green")
		   (change-title))
		  ((= data "interrupted")
		   (notify "INTERRUPTED" "red"))
		  ;; set package
		  ((and (instanceof data |Array|)
			(= (first data) "set-package"))
		   (setf current-package (getprop data 1))
		   (change-title)))))
    
    ;; =========================================================
    ;; Now you need to render results in the result area
    ;; render-result pairs with "build-message-to-send' in server.lisp
    ;; =========================================================
    ;; don't worry about cell focusing here
    ;; resul-area is a div elementp
    (defun render-result (cell result-area evaled-result)
      ((getprop rendering-function-set (@ evaled-result type))
       cell result-area
       (@ evaled-result value) (@ evaled-result stdout)))
    
    ;; Register result rendering functions
    
    ;; Text rendering
    (setf (getprop rendering-function-set "text")
	  (lambda (cell result-area value stdout)
	    (clear-result-area result-area)
	    (setf (chain result-area |innerHTML|)
		  (if (= stdout "")
		      value
		      (chain "{0}<br>{1}" (format stdout value))))))
    
    ;; Draw chart using "flot"
    (setf (getprop rendering-function-set "chart")
	  (lambda (cell result-area value stdout)
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
	      ;; align at the center
	      (draw-chart inner-div (@ value series-list) (@ value options))
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
	  (lambda (cell result-area value stdout)
	    (get-it-focused cell)
	    ;; code generation expression itself is overwritten.
	    ;; I've given a lot of thought into it
	    ;; So dont' change this instantly
	    (chain (getprop cell 'editor) (get-doc) (set-value (first value)))
	    (loop for v1 in (rest value) do
		 (let ((c1 (make-cell focused-cell)))
		   (chain (getprop c1 'editor) (get-doc) (set-value v1))))))
    
    ;; Render error message
    (setf (getprop rendering-function-set "error")
	  (lambda (cell result-area value stdout)
	    (clear-result-area result-area)
	    (setf (chain result-area |innerHTML|)
		  (chain "<font color='red'>{0}</font>" (format value)))))

    ;; Sometimes you may want to render multiple results in a single
    ;; result area, especially for plots.
    ;; You may want to pack up multple plots
    
    ;; todo
    ;; Horizontal overflow scrolling failed,
    ;; try it later.
    ;; I haven't figured out yet which one is better.
    (setf (getprop rendering-function-set "vpack")
	  (lambda (cell result-area value stdout)
	    (clear-result-area result-area)
	    ;; render standard output first
	    (unless (= stdout "")
	      (let ((p (chain document (create-element "p"))))
		(chain result-area (append-child p))
		(setf (chain p |innerHTML|) stdout)))
	    (loop for v1 in value do
		 (let ((div (chain document (create-element "div"))))
		   (chain result-area (append-child div))
		   (render-result cell div v1)))))
    
    (setf (getprop rendering-function-set "hpack")
	  (lambda (cell result-area value stdout)
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
		   (render-result cell div h1)))))

    ;; if editor content starts with ##rawtext the rest is just considered
    ;; as a simple string
    (setf (getprop rendering-function-set "##rawtext")
	  (lambda (cell result-area value stdout)
	    (clear-result-area result-area stdout)
	    ;; hide text area and cell-loc-area
	    (setf (chain (getprop cell 'editor) (get-wrapper-element)
			 style display) "none")
	    (setf (chain (getprop cell 'cell-loc-area) style visibility) "hidden")
	    (setf (chain result-area |innerHTML|) value)
	    ;; Invoke MathJax
	    ((chain |MathJax| |Hub| |Queue|)
	     (array "Typeset" (chain |MathJax| |Hub|) result-area))))
        
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

	(setf (chain div-outer class-name) "cell-div-outer")
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
	
	;; when you type in anything, turn file name to gray
	;; Is this too much?
	(chain editor
	       (on "change"
		   (lambda ()
		     (setf (chain rename-span style color) "gray")
		     ;; Remove system error message if exists
		     (unless (processing-p)
		       (notify "" "")))))
	
	(setf (chain rename-span style color) "gray")
	
	;; Disable ctrl-enter in the editor
	;; it is going to be used as cell evaluation
	;; if you don't include the following expression
	;; the editor will insert a newline for ctrl-enter
	(chain editor (set-option
		       "extraKeys"
		       (create "Ctrl-Enter" (lambda (cm) ))))

	;; Just include them all for convenience
	(let ((cell (create no cell-counter
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
	  ;; double click shows up text area if it's not shown.
	  (let ((textarea-recovered (chain editor (get-wrapper-element))))
	    (setf (chain div-outer ondblclick)
		  (lambda ()
		    (setf (chain cell-loc-area style visibility) "")
		    (setf (chain textarea-recovered style display) ""))))
	  ;; finally return the cell
	  cell)))

    ;; ====================================================
    ;; misc
    ;; ====================================================
    ;; 'msg' is an array of strings, with a command as the first element
    ;; (it can be commands later but just a command yet)
    (defun send-message (msg)
      (chain ws (send (chain +JSON+
			     ;; List may not be the best
			     ;; data structure for this job
			     ;; I just want to simplify it though.
			     (stringify msg)))))
    ;; cellno is an integer
    (defun find-cell (cellno)
      (let ((found nil))
	(loop for cell in all-cells do
	     (when (= cellno (@ cell 'no))
	       (setf found cell)
	       (return)))
	found))
    
    (defun focus-to-next-cell (cell)
      (let ((cell-to-focus
	     ;; if the given cell is the last cell and the content is empty
	     ;; then the given cell is to be focused,
	     ;; if it is simply the last cell then make a new cell at the end
	     ;; and focus it to the newly made cell
	     ;; or else the next cell is focused
	     (cond ((and (last-cell-p cell) (empty-cell-p cell)) cell)
		   ((last-cell-p cell) (make-cell))
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
      (turn-on-border cell)
      (auto-scroll))

    (defun auto-scroll ()
      (let* ((div (getprop focused-cell 'div-outer))
	     (divp (chain div (get-bounding-client-rect))))
	;; unless the focused-cell is not in view
	(unless (and (>= (@ divp top) cell-pad-top-position)
		     (<= (@ divp bottom) (chain ($ document) (height))))
	  (chain div (scroll-into-view)))))

    ;; notify at notice area
    ;; (notify "" "") is a thawing potion.
    ;; If the content of notice element is "PROCESSING",
    ;; most of the user actions are prohibitted.
    ;; see processing-p 
    (defun notify (message color-string)
      (setf (chain notice |innerHTML|) message)
      (setf (chain notice style color) color-string))

    (defun processing-p ()
      (= (chain notice |innerHTML|) "PROCESSING"))
    
    ;; not a very efficient way but
    ;; I don't want to bother.
    ;; computers are fast enough
    (defun refresh-cell-loc ()
      (loop for i from 1
	 for cell in all-cells do
	   (setf (chain (getprop cell 'cell-loc-area) |innerHTML|)
		 (pad (chain i (to-string)) 3)))
      (setf (chain rename-span style color) "Gray")
      (unless (processing-p)
	(notify "" "")))

    ;; ====================================================
    ;; Helpers
    ;; ====================================================
    ;; "flot" specific
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
	      (+ current-package " " (chain rename-span |innerHTML|)))))

    (defun clear-result-area (result-area)
      (setf (chain result-area |innerHTML|) ""))

    ;; ===========================================
    ;; On page loading 
    ;; ===========================================
    
    (defun init ()
      ;; rename-span is for broadcasting, sort of.
      ;; Hence used often
      (setf rename-span (chain document (get-element-by-id "rename_span")))
      ;; rename span value includes spaces so must be trimmed first
      (setf (chain rename-span |innerHTML|)
	    (chain rename-span |innerHTML| (trim)))

      (setf notice (chain document (get-element-by-id "notice")))
      
      ;; set current-package
      (setf current-package (lisp *default-working-package*))
      ;; Change title value to file name
      (change-title)

      ;; add key event ctrl-return to eval the focused ce
      (chain ($ document)
	     (keydown (lambda (event)
			(when (and (chain event ctrl-key)
				   ;; 13 represents "Enter key"
				   (= (chain event which) 13))
			  (eval-cell focused-cell "evalkey")))))

      ;; add key event ctrl-s to save notebook
      (chain ($ document)
	     (keydown (lambda (event)
			(when (and (chain event ctrl-key)
				   ;; 83 represents "s"
				   (= (chain event which) 83))
			  (save-notebook)))))
      ;; cell-pad
      (setf cell-pad (chain document (get-element-by-id "cellpad")))
      (let ((cell (make-cell)))
	(setf cell-pad-top-position (chain (getprop cell 'div-outer) offset-top)))
      
      ;; websocket setup
      (setf ws (new (|WebSocket|
		     (lisp
		      (format nil "ws://127.0.0.1:~A~A"
			      *web-socket-port* *ws-loc*)))))

      ;; when notebook-file is given as an argument
      (setf (chain ws onopen)
	    (lambda ()
	      (when (lisp notebook-filename)
		;; loading file requires a websocket opened
		(load-notebook-file (lisp notebook-filename)))
	      (chain console (log "Openning connection to websocket"))))
      
      (setf (chain ws onmessage)
	    (lambda (event)
	      (handle-message (chain event data)))))
    ;; 
    (setf (chain window onload) init)
    (setf (chain window onbeforeunload)
	  (lambda () (save-notebook) "Closing Current Notebook"))))



;; page html
(defparameter *menubutton-size* "40px")

(defun notebook-page (&optional notebook-filename)
  (let ((untitled (string (gensym "untitled"))))
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

	(:script
	 :type "text/x-mathjax-config"
	 (ps ((chain |MathJax| |Hub| |Config|)
	      (create tex2jax (create inline-math (array (array "$" "$")
							 (array "\\(" "\\)"))
				      display-math (array (array "$$" "$$")
							  (array "\\[" "\\]"))
				      process-escapes true)
		      "HTML-CSS" (create available-fonts (array "TeX")))))

	 )
	(:script
	 :type "text/javascript"
	 ;; Mathjax
	 ;; Must be online to use latex
	 :src "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
	(:script :type "text/javascript" (str (js-for-notebook-page notebook-filename)))
	(:title :id "title" "YOMI"))

       (:body
	(:div :id "notebook-menu"
	      :style " background-color: #FFFFFF; overflow:hidden"
	      (:div :style "height:0px; overflow:hidden"
		    ;; open in new tab
		    (:form
		     ;; opening new-tabs can be problematic sometimes
		     ;; :target "_blank"
		     :action "/yomi" :method "post"
		     :enctype "multipart/form-data"
		     (:input
		      :type "file" :id "choose-notebook"
		      :name "yomifile"
		      :onchange
		      (ps (chain this form (submit))))))
	      ;; menu icons	    
	      (:div :style "display: inline-block"
		    (:input :type "image" :src "folder.png"
			    :width *menubutton-size* :height *menubutton-size*
			    ;; save it just in case
			    :onclick (ps (save-notebook) (choose-notebook)))
		    (:input :type "image" :src "notebook.png"
			    :width *menubutton-size* :height *menubutton-size*
			    :onclick (ps (open-in-new-tab "yomi")))
		    "&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; "
		    (:input :type "image" :src "play.png"
			    :width *menubutton-size* :height *menubutton-size*
			    :onclick (ps (eval-cell focused-cell)))
		    (:input :type "image" :src "fast_forward.png"
			    :width *menubutton-size* :height *menubutton-size*
			    :onclick (ps (eval-cell focused-cell "evalon")))
		  
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
	      "&nbsp; &nbsp; &nbsp; &nbsp; "
	      (:span  :id "notice"
		      :style "font-size:100%;color:gray;" "")
	    
	      (:div :id "rename_div"
		    :style "display:inline-block; position:absolute; right: 0;margin-right:14px"

		    ;; show system messages here
		    (:span :style "font-size:100%;color:gray;"
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
			      (when (= (@ event key-code) 13)
				(if (= (chain this value (trim)) "")
				    (alert "Please Enter a Name")
				    (chain document
					   (get-element-by-id "rename_button")
					   (click))))))
		  
		    (:input :type "submit"
			    :style "display:none;"
			    :onclick (ps (rename-notebook)) 
			    :id "rename_button")
		  
		    (:input :type "image" :src "save.png"
			    :width *menubutton-size* :height *menubutton-size*
			    :onclick (ps (save-notebook)))))

	(:div :id "cellpad"))))))



