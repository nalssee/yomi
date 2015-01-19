(in-package :yomi)

;; About Chart

(defclass chart ()
  ((title :initarg :title :initform "Chart")
   (width :initarg :width :initform 400)
   (height :initarg :height :initform 225)
   (series-list :initarg :series-list :initform (error "series-list must be supplied"))
   ;; encode-json turns a list into json object
   ;; options can be very complex
   (options :initarg :options))
  
  (:documentation "FLOT Chart"))


(defclass series ()
  (
   ;; points instance
   (label :initarg :label :initform (error "label must be supplied"))
   ;; data: '((1 2) (3 4) ...)
   ;; vector or list
   (data :initarg :data :initform (error "data must be supplied"))
   (points :initarg :points
	   :initform (make-instance 'points)
	   :accessor series-points)
   ;; default: no lines
   (lines :initarg :lines
	  :initform '((show . nil)))
   (bars :initarg :bars
	 :initform '((show . nil)))

   (color :initarg :color :initform "red"
	  :accessor series-color)
   
   ;; point radius
   )
  (:documentation "Scatter Series for FLOT"))



(defclass points ()
  ;; "triangle", "square",  "circle", "diamond", "cross"
  ((symbol :initarg :symbol :initform "circle" :accessor points-symbol)
   ;; show points always
   (show :initarg :show :initform t)
   (fill-color :initarg :fill-color :initform "red" :accessor points-fill-color)))






(defun plot (&rest xs)
  (let ((series-list (remove-if-not
		      (lambda (x) (eql (type-of x) 'series))
		      xs))
	(kv-pairs (remove-if-not
		   (lambda (x) (not (eql (type-of x) 'series)))
		   xs)))
    (format (load-time-value *standard-output*) "~%~A" series-list)
    (let ((title (getf kv-pairs :title))
	  (width (or (getf kv-pairs :width) 400))
	  (height (or (getf kv-pairs :height) 225))
	  ;; default radius for a point is 1
	  (radius (or (getf kv-pairs :radius) 1))
	  (xlabel (getf kv-pairs :xlabel))
	  (ylabel (getf kv-pairs :ylabel))
	  (xrange (getf kv-pairs :xrange))  ; '(-10 10)
	  (yrange (getf kv-pairs :yrange))) ; '(-4 12)
      (let ((options `((axis-labels . ((show . t)))
		       (xaxes . (((axis-label . ,xlabel))))
		       (yaxes . (((axis-label . ,ylabel))))
		       (series . ((points . ((radius . ,radius)
					     (show . t)
					     (fill . t))))))))
	(when xrange
	  (push `(xaxis . ((min . ,(first xrange))
			   (max . ,(second xrange))))
		options))
	(when yrange
	  (push `(yaxis . ((min . ,(first yrange))
			   (max . ,(second yrange))))
		options))
	(make-instance 'chart :title title
		       :width width
		       :height height
		       :series-list series-list
		       :options options)))))


;; symbols
;; circle, diamond, triangle
(defun series (data &key
		      label
		      (color "red" color-p)
		      (symbol "circle")
		      ;; no lines default
		      (points t)
		      (lines nil)
		      (bars nil))
  "symbol: circle, diamond, triangle, square, cross
   as for colors refer to *color-list* variable
  "
  (unless color-p
    (setf color (nth (random (length *color-list*)) *color-list*)))
  (make-instance 'series
		 :label label
		 ;; other data types will be included
		 :data data
		 :points (make-instance 'points
					:symbol symbol
					:show points
					;; fill-color and color
					;; are better be the same
					:fill-color color)
		 :color color
		 :lines `((show . ,lines))
		 :bars `((show . ,bars))

		 ))

;; data is a list of numbers
;; '(3 2 3 19 2 3 4)
(defun hist (data &key
		    label
		    (color "red" color-p)
		    (symbol "circle")
		    ;; no lines default
		    (points nil)
		    (lines nil))
  (unless color-p
    (setf color (nth (random (length *color-list*)) *color-list*)))
  (make-instance 'series
		 :label label
		 ;; other data types will be included
		 :data (freq data)
		 :points (make-instance 'points
					:symbol symbol
					:show points
					;; fill-color and color
					;; are better be the same
					:fill-color color)
		 :color color
		 :lines `((show . ,lines))
		 :bars `((show . t) (fill-color . ,color))
		 ))



(defun freq (data)
  (let ((table (make-hash-table)))
    (loop for x in data do
	 (incf (gethash x table 0)))
    (let (result)
      (maphash #'(lambda (k v) (push (list k v) result)) table)
      (sort result #'< :key #'first))))





