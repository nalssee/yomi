#|
Yomi package
|#
(defpackage :yomi
  (:use :cl :parenscript :cl-who :hunchentoot :clws :cl-fad)
  (:export
   :start-yomi
   :make-series
   :make-chart


	   ))


;; package for yomi notebook
(defpackage :ynb
  (:use :cl :yomi)
  (:shadow :start-yomi))



