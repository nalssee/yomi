#|
Yomi package
|#
(defpackage :yomi
  (:use :cl :parenscript :cl-who :hunchentoot :clws :cl-fad)
  (:export
   :start-yomi
   :make-series
   :make-chart
   :*working-directory*
   :*max-eval-threads*
   :*keymap*
   :demo
   ))


;; package for yomi notebook
(defpackage :ynb
  (:use :cl)
  (:import-from
   :yomi
   :make-chart
   :make-series
   :demo
   ))
