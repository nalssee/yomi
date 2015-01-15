#|
Yomi package
|#
(defpackage :yomi
  (:use :cl :parenscript :cl-who :hunchentoot :clws :cl-fad)
  (:export
   :start-yomi
   :make-series
   :make-chart
   :*max-eval-threads*
   :*keymap*
   ))


;; package for yomi notebook
(defpackage :ynb
  (:use :cl)
  (:import-from
   :yomi
   :make-chart
   :make-series))
