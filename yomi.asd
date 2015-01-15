

(asdf:defsystem :yomi
  :author "Kenjin Che <kenjin@sdf.org>"
  :depends-on (:cl-who
	       :hunchentoot
	       :clws
	       :parenscript
	       :cl-json
	       #-(AND DARWIN SBCL)
	       :inferior-shell
	       :cl-fad
	       :bordeaux-threads
	       :usocket

	       )

  :serial t
  :components (
	       (:file "packages")
	       (:file "parameters")
	       (:file "plot")
	       (:file "client")
	       (:file "server")
	       
	       )

  )


