(in-package :yomi)

(defclass code ()
  ((codelist :initarg :codelist
	     :reader codelist)))

(defun code->string (code)
  (if (and (listp code) (eql (first code) 'progn))
      (format nil "~{~S~^~%~}" (rest code))
      (format nil "~S" code)))

(defun make-code (codelist)
  (make-instance
   'code
   :codelist
   (mapcar #'code->string
	   (progn (let (result)

		    #+SBCL
		    (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
		      (SETQ *PACKAGE*
			    (SB-INT:FIND-UNDELETED-PACKAGE-OR-LOSE *DEFAULT-WORKING-PACKAGE*)))
		    #+CCL
		    (EVAL-WHEN (:EXECUTE :LOAD-TOPLEVEL :COMPILE-TOPLEVEL)
		      (CCL::SET-PACKAGE *DEFAULT-WORKING-PACKAGE*))
		    (setf result codelist)
		    (in-package :yomi)
		    result)))))

(defun demo ()
  (make-code
   '(
     (+ 3 4)
     (format t "Standard~%    Output")
     (intentional error)
     
     (plot (series (loop repeat 100 collect (list (random 10.0)
						  (random 20.0)))))
     ;; 
     (progn
       (format t "sin(x) and cos(x) from 0 to 2*pi")
       (plot (series (loop for i from 0 to (* 2 pi) by 0.1 collect
			  (list i (sin i))) :lines t :points nil :color "blue"
			  :label "sin(x)")
	     (series (loop for i from 0 to (* 2 pi) by 0.1 collect
			  (list i (cos i))) :lines t :points nil :color "red"
			  :label "cos(x)")
	     :xlabel "Radian"))

     
     (plot
      (series '((1 2) (2 3) (5 2) (9 4))
       :lines t
       :symbol "cross"
       :label "data 1"
       :color "green")
      (series (loop repeat 20 collect (list (random 10.0) (random 5.0)))
       :label "data 2" :color "purple")
      :title "Sample Chart" :width 600 :height 200
      :xlabel "x axis" :ylabel "y axis" :xrange '(0 12)
      :yrange '(0 6))

     

     (progn
       (defun throw-coins (n)
	 (loop repeat n collect (random 2)))
       (defun count-ones (xs)
	 (length (remove-if-not #'(lambda (x) (= x 1)) xs)))
       (let ((xs (loop repeat 3000 collect
		      (count-ones (throw-coins 100)))))
	 (plot (hist xs :label "n of front")
	       :width 700 :height 300
	       :title "Coin Throw"
	       :xlabel "out of 100"
	       :ylabel "out of 3000")))
     
     
     (progn
       (defun samplot (width height)
	 (plot (series '((-2 2) (3 4) (5 -2) (8 0)) :lines t)
	       :width width :height height :title "FOO"))
       (format  t "Packing Exmaple")
       (pack (list (pack (samplot 300 100)
			 (samplot 300 100))
		   (samplot 180 233)
		   (samplot 200 233))
	     (samplot 674 100)))
     (progn
      (ql:quickload "alexandria")
      (defpackage :foo
	(:use :cl :yomi :alexandria))
      (set-package :foo)
      (format t "You can use functions in alexandria package once this cell is evaluated"))
     
     (flatten '((3) 4 ((5 (6)) 7)))

     (progn
       (format t "Set it back to ynb")
       (set-package :ynb))

     (progn
       (format t "And flatten can't be used directly anymore")
       (format t "~%So you will see an error message")       
       (flatten '((3) 4)))
     
     
     )))



