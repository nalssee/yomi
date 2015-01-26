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
   :vpack
   :hpack
   ;; for flot option creation.
   :create

   :set-package
   :make-code
   :append-code
   ))

;; Using yomi package directly seems dangerous.
(defpackage :ynb
  (:use :cl :yomi))

