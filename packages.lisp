(defpackage :yomi
  (:use :cl :parenscript :cl-who :hunchentoot :clws :cl-fad)
  (:export
   :start-yomi
   :demo

   :cd
   :ls
   :keymap
   
   :plot
   :series
   :hist
   :packv
   :packh
   ;; for flot option creation.
   :create

   :set-package
   :defcode
   ))

;; Using yomi package directly seems dangerous.
(defpackage :ynb
  (:use :cl :yomi))


