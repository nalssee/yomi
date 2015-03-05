(in-package :yomi)

(defclass code ()
  ((codelist :initarg :codelist
	     :reader codelist)))

;; I believe this is more flexible and intuitive
;; than providing a macro
(defun make-code (&rest sexp-codes)
  (make-instance
   'code
   :codelist
   (mapcar #'code->string
	   (progn (let (result)
		    ;; Think when you use make-code in other packages
		    #+SBCL
		    (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
		      (SETQ *PACKAGE*
			    (SB-INT:FIND-UNDELETED-PACKAGE-OR-LOSE *DEFAULT-WORKING-PACKAGE*)))
		    #+CCL
		    (EVAL-WHEN (:EXECUTE :LOAD-TOPLEVEL :COMPILE-TOPLEVEL)
		      (CCL::SET-PACKAGE *DEFAULT-WORKING-PACKAGE*))
		    (setf result (remove nil sexp-codes))
		    (in-package :yomi)
		    result)))))

(defun append-code (&rest codes)
  "Append instances of code class"
  (make-instance
   'code
   :codelist (mapcan #'codelist codes)))

(defun code->string (code)
  (cond ((stringp code) code)
	((and (listp code) (eql (first code) 'progn))
	 ;; progn is stripped off
	 (format nil "~{~S~^~%~}" (peel-package-name (rest code))))
	(t (format nil "~S" (peel-package-name code)))))

;; todo 
;; a bit heavy 
(defun peel-package-name (code)
  "Showing a package name for each symbol looks ugly
   So, strip it off"
  (cond ((atom code)
	 ;; If code is a symbol and it is a symbol in *default-working-package*
	 ;; only symbol name is stringified
	 (if (and (symbolp code) (string= *default-working-package*
					  (package-name (symbol-package code))))
	     (intern (symbol-name code))
	     code))
	(t (cons (peel-package-name (first code))
		 (peel-package-name (rest code))))))

;; 
(defun demo ()
  (make-code
   "##rawtext
<h2><font color='dodgerblue'>Simple Expressions</font></h2>"
   '(+ 3 4)
   '(format t "Standard~%    Output")
   '(intentional error)
   "##rawtext
<h2><font color='dodgerblue'>Plot Examples</font></h2>"
   ;; 
   '(progn
     (format t "sin(x) and cos(x) from 0 to 2*pi")
     (splot
      (series (loop for i from 0 to (* 2 pi) by 0.1 collect
		   (list i (sin i))) :type "line" :name "sin(x)")
      (series (loop for i from 0 to (* 2 pi) by 0.1 collect
		   (list i (cos i))) :type "line" :name "cos(x)")
      :x-label "Radian"
      :point "hide"))
   
   '(progn
     (defun throw-coins (n)
       (loop repeat n collect (random 2)))
     (let ((xs (loop repeat 3000 collect
		    (count 1 (throw-coins 100)))))
       (splot (hist xs :name "n front")
	      :width 700 :height 300)))

   "##rawtext
<h2><font color='dodgerblue'>Multiple Results (not just for plots)</font></h2>"
   '(progn
     (defun samplot (width height)
       (splot (series '((-2 2) (3 4) (5 -2) (8 0)) :type "line")
	      :width width :height height :legend "hide"))
     (format  t "Packing Exmaple")
     (vpack (hpack (vpack (samplot 300 200)
			  (samplot 300 200))
	     (samplot 400 414))))

   "##rawtext
<h2><font color='dodgerblue'>SET-PACKAGE</font></h2>
<font color='orange'>A page title indicates a current package.</font>"
   '(progn
     (ql:quickload "alexandria")
     (defpackage :foo
       (:use :cl :yomi :alexandria))
     (set-package :foo))
   "##rawtext
<font color='orange'>Now that a current package is set to \"foo\",</font>"
   '(flatten '((3) 4 ((5 (6)) 7)))
   "##rawtext <font color=\"orange\">Set it back to \"YNB\". </font>"
   '(set-package :ynb)

   "##rawtext
<font color='orange'>You should see an error now.</font>"
   '(flatten '((3) 4))
   
   "##rawtext
<h2><font color='dodgerblue'>MathJax</font></h2>
You need to be online to get the next cell shown properly.
"
   "##rawtext
This is text style: \\(lim_{n \\to \\infty} \\sum_{k=1}^n \\frac{1}{k^2} = \\frac{\\pi^2}{6} \\).
And this is display style:
$$ lim_{n \\to \\infty} \\sum_{k=1}^n \\frac{1}{k^2} = \\frac{\\pi^2}{6} $$
"
   "##rawtext
<h2><font color='dodgerblue'>Generate Cells</font></h2>
This is why I bother to write this program.
And this 'demo' function itself is written this way.
Type in the next cell (make-code '(+ 10 20) '(* 10 20))
and press 'fast-forward' button above to see what happens.
"
   ))



