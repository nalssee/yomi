(in-package :yomi)

(defclass chart ()
  ((title :initarg :title)
   (width :initarg :width)
   (height :initarg :height)
   (series-list :initarg :series-list)
   (options :initarg :options)))

(defclass series ()
  ((label :initarg :label)
   ;; data: '((1 2) (3 4) ...)
   (data :initarg :data)
   (points :initarg :points)
   (lines :initarg :lines)
   (bars :initarg :bars)
   (color :initarg :color)))

(defun plot (&rest xs)
  (let ((series-list (remove-if-not
		      (lambda (x) (eql (type-of x) 'series))
		      xs))
	(kv-pairs (remove-if-not
		   (lambda (x) (not (eql (type-of x) 'series)))
		   xs)))
    (let ((options (create :axis-labels (create :show t))))
      (loop for kvs on kv-pairs by #'cddr do
	   (let ((key (first kvs))
		 (value (second kvs)))
	     (case key
	       ;; option adjoining
	       (:xlabel (setf options
			      (adjoin-option
			       :xaxes (list (create :axis-label value)) options)))
	       (:ylabel (setf options
			      (adjoin-option
			       :yaxes (list (create :axis-label value)) options)))
	       (:xrange (setf options
			      (adjoin-option :xaxis (create :min (first value)
							    :max (second value))
					     options)))
	       (:yrange (setf options
			      (adjoin-option :yaxis (create :min (first value)
							    :max (second value))
					     options)))
	       ((:title :width :height) 'no-nothing)
	       ;; some other options
	       (otherwise (adjoin-option key value options)))))
      (make-instance 'chart
		     :title (getf kv-pairs :title)
		     :width (or (getf kv-pairs :width) 400)
		     :height (or (getf kv-pairs :height) 225)
		     :series-list series-list
		     :options options))))

(defun series (data &key
		      label
		      (color (pick-color))
		      (points t)
		      (lines nil)
		      (bars nil))
  (make-instance 'series
		 :label label
		 :data data
		 :points (adjoin-option :fill-color color
					(t-nil points))
		 :color color
		 :lines (t-nil lines)
		 :bars (t-nil bars)))


(defun pick-color ()
  (nth (random (length *color-list*)) *color-list*))

(defun adjoin-option (key value options)
  (if (assoc key options)
      (progn (rplacd (assoc key options) value)
	     options)
      (push (cons key value) options)))

(defun create (&rest kv-pairs)
  "Remember create from parenscript?"
  (loop for xs on kv-pairs by #'cddr collect
       (cons (first xs) (second xs))))

(defun t-nil (x)
  (case x
    ((t nil) (create :show x))
    (otherwise x)))

;; data is a list of numbers
;; '(3 2 3 19 2 3 4)
(defun hist (data &key label (color (pick-color)))
  (series (freq data) :color color
	  :label label :bars (create :show t :fill-color color) :points nil))

(defun freq (data)
  (let ((table (make-hash-table)))
    (loop for x in data do
	 (incf (gethash x table 0)))
    (let (result)
      (maphash #'(lambda (k v) (push (list k v) result)) table)
      (sort result #'< :key #'first))))
