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
   '(plot (series '((-2 2) (3 4) (5 -2) (8 0))))
   ;; 
   '(progn
     (format t "sin(x) and cos(x) from 0 to 2*pi")
     (plot (series (loop for i from 0 to (* 2 pi) by 0.1 collect
			(list i (sin i))) :lines t :points nil :color "blue"
	    :label "sin(x)")
      (series (loop for i from 0 to (* 2 pi) by 0.1 collect
		   (list i (cos i))) :lines t :points nil :color "red"
       :label "cos(x)")
      :xlabel "Radian"))

   '(PLOT (SERIES
	   (LOOP REPEAT 30
	      COLLECT (LET ((X (RANDOM 10.0)))
			(LIST X (- (+ (* X 0.5) (RANDOM 2.0)) 1))))
	   :LABEL "Random Nums" :COLOR "purple" :POINTS (CREATE :SHOW T :RADIUS 5))
     (SERIES '((0 0) (10 5)) :LINES T :LABEL "Pseudo L-Sq fit" :COLOR "green")
     :TITLE "Pseudo Least Squares Fit" :WIDTH 600 :HEIGHT 200 :XLABEL "X-val" :YLABEL
     "Y-val" :XRANGE '(0 11) :YRANGE '(0 5.5))

   '(progn
     (defun throw-coins (n)
       (loop repeat n collect (random 2)))
     (let ((xs (loop repeat 3000 collect
		    (count 1 (throw-coins 100)))))
       (plot (hist xs :label "n of front")
	     :width 700 :height 300
	     :title "Binary Distribution"
	     :xlabel "n of front")))
   "##rawtext
<h2><font color='dodgerblue'>Multiple Results (not necessarily plots)</font></h2>"
   '(progn
     (defun samplot (width height radius &optional (point-shape "circle"))
       (plot (series '((-2 2) (3 4) (5 -2) (8 0)) :lines t
		     :points (create :show t :radius radius :symbol point-shape))
	     :width width :height height :title point-shape))
       
     (format  t "Packing Exmaple")
     (vpack (hpack (vpack (samplot 300 100 8 "cross")
			  (samplot 300 100 8 "diamond"))
	     (samplot 180 236 8 "triangle")
	     (samplot 200 236 8 "square"))
      (samplot 680 100 8)))

   '(progn
     (defun p1 (x)
       (plot (series (loop for i from 1 to 100 by (* x pi)
			collect (list (sin i) (cos i)))
		     :lines t)
	     :width 150 :height 150 :yrange '(-1 1)
	     :xrange '(-1 1)))
     (vpack (hpack (p1 0.2) (p1 0.3) (p1 0.6))
      (hpack (p1 0.7) (p1 0.8) (p1 1.1))))

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
   ))
