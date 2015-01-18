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
		    (in-package :ynb)
		    (setf result codelist)
		    (in-package :yomi)
		    result)))))

(defun demo ()
  (make-code
   '(
		
     (make-chart (list (make-series '((2 3) (1 2) (5 2) (9 4)) :bars t :points nil))
      :width 300
      :height 200)
		
     (+ 3 4)
		
     (progn
       (defun fib (n)
	 (if (< n 2)
	     n
	     (+ (fib (- n 1)) (fib (- n 2)))))
       (make-chart
	(list (make-series (let ((fib20 (fib 20)))
			     (loop repeat 100 collect (list (random fib20) 0)))
			   :color "blue"
			   :label "random")
	      (make-series (list (list (fib 20) 1))
			   :color "green"
			   :label "true"))
	:width 800
	:height 200))
     )
   
   )
  )
















