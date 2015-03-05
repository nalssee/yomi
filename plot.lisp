(in-package :yomi)

;; It is highly likely that I regret if I fix ploting interface at this moment of time.
;; So, I just decided to generate a JSON object string so the client can parse to
;; generate a plot based on the information.
;; This only property that you don't have to worry about is "bindto",
;; which is going to be handled by the client

;; One anther advantage of tis approach is that I don't have to make a document for this.
;; Simply go to http://c3js.org
(defclass chart ()
  ((chart-object-string :initarg :chart-object-string
			:reader chart-object-string)))

(defun plot (xs)
  (make-instance 'chart :chart-object-string
		 (my-encode-json-plist xs)))

;; It's annoying but I have to write a simple json encoder for plist myself
;; Since cl-json doesn't work correctly for nested ones
;; and yason converts keys to upcase string and no way around it is provided.
;; If you convert :abc to string it is upcased. So let's just assume that
;; all the symbols and strings for naming are downcased in this plot making.
;; You can't make a label like "Foo". It should be just "foo"
(defun my-encode-json-plist (xs &key (key-converter #'string-downcase))
  "plist -> JSON object string"
  (cond ((stringp xs) (format nil "~S" xs))
	((numberp xs) (if (integerp xs)
			  ;; try (format nil "~A" pi)
			  ;; and you will see why the following.
			  (format nil "~d" xs)
			  (format nil "~f" xs)))
	((atom xs)
	 ;; handle some specieal symbols
	 (case xs
	   (:true "true")
	   (:false "false")
	   (otherwise (format nil "~A" xs))))
	((plist? xs)
	 (format nil "{~{~A~^, ~}}"
		 (loop for (a . as) on xs by #'cddr collect
		      (format nil "~S: ~A" (funcall key-converter (string a))
			      (my-encode-json-plist (first as) :key-converter key-converter)))))
	((listp xs)
	 (format nil "[~{~A~^, ~}]"
		 (loop for x in xs collect
		      (my-encode-json-plist x :key-converter key-converter))))))

(defun plist? (xs)
  (and (listp xs) (evenp (length xs)) 
       (loop for (a . nil) on xs by #'cddr always (keywordp a))))

;; Users don't need to worry about internal name at all.
(defstruct series data type name internal-name)

;; There can be more options
;; Actually just a renaming.
(defun series (data &key name (type "scatter"))
  (let ((internal-name (string-downcase (string (gensym "DATA")))))
    (make-series :data data :type type
		 :internal-name internal-name
		 :name (or name internal-name))))

;; Provide a simple template plot
(defun splot (&rest xs)
  ;; Funtions defined under flet are not structured very well.
  ;; Since these are flet functions
  ;; (close together and do not affect others)
  ;; and they don't have to be efficient at all,
  ;; I hope it is at least acceptable, more readable if any.
  (flet ((make-xs (series)
	   (loop for s in series nconc
		(let ((iname (series-internal-name s)))
		  (list (intern (string-upcase iname) :keyword)
			(concatenate 'string iname "_x")))))
	 (make-columns (series)
	   (loop for s in series nconc
		(let ((iname (series-internal-name s))
		      (data (series-data s)))
		  (list (cons (concatenate 'string iname "_x") (mapcar #'first data))
			(cons iname (mapcar #'second data))))))
	 (make-types (series)
	   (loop for s in series nconc
		(list (intern (string-upcase (series-internal-name s)) :keyword)
		      (series-type s))))
	 (make-names (series)
	   (loop for s in series nconc
		(list (intern (string-upcase (series-internal-name s)) :keyword)
		      (series-name s)))))
    ;; Acturally a list serieses
    (let ((series (remove-if-not #'(lambda (x)
				     (eql (type-of x) 'series)) xs))
	  (kv-pairs (remove-if #'(lambda (x)
				   (eql (type-of x) 'series)) xs)))
      (let ((x-label (or (getf kv-pairs :x-label) "x"))
	    (y-label (or (getf kv-pairs :y-label) "y"))
	    (width (getf kv-pairs :width))
	    (height (getf kv-pairs :height))
	    ;; legend can be "bottom" "right" or "hide"
	    (legend (or (getf kv-pairs :legend) "bottom"))
	    (point (or (getf kv-pairs :point) "show")))
	(plot
	 (append
	  (list :data (append (list :xs (make-xs series)
				    :columns (make-columns series)
				    :types (make-types series)
				    :names (make-names series)))
		:axis (list :x (list :label x-label
				     ;; no idea what it is yet
				     ;; just copied from http://c3js.org
				     :tick (list :fit :false))
			    :y (list :label y-label))
		:legend (if (string= legend "hide")
			    (list :show :false)
			    (list :position legend))
		:point (list :show (if (string= point "show")
				       :true
				       :false)))
	  ;; size might be there or not
	  (when (or width height)
	    (list :size (append (when width
				  (list :width width))
				(when height
				  (list :height height)))))))))))

;; data: '(3 2 3 4 5 5 1)
(defun hist (data &key name)
  (series (freq data) :name name :type "bar"))

(defun freq (data)
  (let ((table (make-hash-table)))
    (loop for x in data do
	 (incf (gethash x table 0)))
    (let (result)
      (maphash #'(lambda (k v) (push (list k v) result)) table)
      (sort result #'< :key #'first))))

