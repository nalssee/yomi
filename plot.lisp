(in-package :yomi)

;; About Chart

(defclass chart ()
  ((title :initarg :title :initform "Chart")
   (width :initarg :width :initform 640)
   (height :initarg :height :initform 480)
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



(defun make-chart
    (series-list &key (title "Simple Chart")
		   (width 640)
		   (height 480)
		   (radius 2)
		   (xlabel "X")
		   (ylabel "Y")
		   xrange		; '(-10 10)
		   yrange		; '(-4 12)

		   )
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
		   :options options))
    
  )




;; symbols
;; circle, diamond, triangle
(defun make-series (data &key
			   (label "data")
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







