(in-package :yomi)

(defclass code ()
  ((codelist :initarg :codelist
	     :reader codelist)))


(defun code->string (code)
  (if (and (listp code) (eql (first code) 'progn))
      (format nil "~{~S~^~%~}" (peel-package-name (rest code)))
      (format nil "~S" (peel-package-name code))))


;; todo 
;; a bit heavy 
(defun peel-package-name (code)
  "Showing a package name for each symbol looks ugly
   So, strip it off"
  (cond ((atom code)
	 (if (and (symbolp code) (string= *default-working-package*
					  (package-name (symbol-package code))))
	     (intern (symbol-name code))
	     code))
	(t (cons (peel-package-name (first code))
		 (peel-package-name (rest code))))))


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



;;
;; <- todo
;; Think further
(defmacro defcode (name args &body body)
  `(defun ,name ,args
     (make-code
      (sublis (mapcar #'cons ',args (list ,@args))
	      ',body))))



(defcode demo ()
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

  	   :label "data 1"
  	   :color "green")
   (series (loop repeat 20 collect (list (random 10.0) (random 5.0)))
  	   :label "data 2" :color "purple")
   :title "Sample Chart" :width 600 :height 200 :radius 5
   :xlabel "x axis" :ylabel "y axis" :xrange '(0 12) 
   :yrange '(0 6))


  (progn
    (defun throw-coins (n)
      (loop repeat n collect (random 2)))
    (let ((xs (loop repeat 3000 collect
  		   (count 1 (throw-coins 100)))))
      (plot (hist xs :label "n of front")
  	    :width 700 :height 300
  	    :title "Coin Throw"
  	    :xlabel "out of 100"
  	    :ylabel "out of 3000")))

  
     
  (progn
    (defun samplot (width height radius &optional (point-shape "circle"))
      (plot (series '((-2 2) (3 4) (5 -2) (8 0)) :lines t
		    :points (create :show t :radius radius :symbol point-shape))
  	    :width width :height height :title point-shape))
       
    (format  t "Packing Exmaple")
    (packv (packh (packv (samplot 300 100 8 "cross")
			 (samplot 300 100 8 "diamond"))
		  (samplot 180 233 6 "triangle")
		  (samplot 200 233 10 "square"))
	   (samplot 674 100 1)))


  
  (progn
    (defun p1 (x)
      (plot (series (loop for i from 1 to 100 by (* x pi)
		       collect (list (sin i) (cos i)))
		    :lines t)
	    :width 150 :height 150 :yrange '(-1 1)
	    :xrange '(-1 1)))
    (packv (packh (p1 0.2) (p1 0.3) (p1 0.6))
	   (packh (p1 0.7) (p1 0.8) (p1 1.1))))
  
  
  (progn
    (ql:quickload "alexandria")
    (defpackage :foo
      (:use :cl :yomi :alexandria))
    (set-package :foo)
    "You can use functions in alexandria package once this cell is evaluated")
     
  (flatten '((3) 4 ((5 (6)) 7)))

  (progn
    "Set it back to ynb"
    (set-package :ynb))

  (progn
    "And flatten can't be used directly anymore"
    "So you will see an error message"
    (flatten '((3) 4)))

  ;; (defcode defcode-example (x)
  ;;   "defcode for interactive simulation, demo function itsef is written this way"
  ;;   (progn
  ;;     (format t "sample plot")
  ;;     (plot (series 'x))))

  ;; (defcode-example '((1 2) (3 4) (5 6) (7 8)))

  
  )



