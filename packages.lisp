(defpackage :yomi
  (:use :cl :parenscript :cl-who :hunchentoot :clws :cl-fad)
  (:export
   :start-yomi
   :demo

   :cd
   :ls
   :keymap
   
   :plot
   :splot
   :series
   :hist
   
   :vpack
   :hpack
   
   :set-package
   :make-code
   :append-code
   ))

;; Using yomi package directly seems dangerous.
(defpackage :ynb
  (:use :cl :yomi))

