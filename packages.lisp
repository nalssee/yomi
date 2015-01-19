#|
Yomi package
|#
(defpackage :yomi
  (:use :cl :parenscript :cl-who :hunchentoot :clws :cl-fad)
  (:export
   :start-yomi
   :series
   :plot
   :hist
   :*working-directory*
   :*max-eval-threads*
   :*keymap*
   :demo
   :cd
   :pack
   :keymap
   
   ))



;; package for yomi notebook
(defpackage :ynb
  (:use :cl)
  (:import-from
   :yomi
   :plot
   :series
   :hist
   :demo
   :cd
   :pack
   :keymap
   ))

