(asdf:defsystem :yomi
  :author "Kenjin Che <kenjin@sdf.org>"
  :depends-on (:hunchentoot
	       :clws
	       :bordeaux-threads
	       :usocket
	       #-(AND DARWIN SBCL)
	       :inferior-shell
	       :cl-fad

	       :parenscript
	       :cl-who
	       :cl-json)
  :serial t
  :components ((:file "packages")
	       (:file "parameters")

	       (:file "plot")
	       (:file "defcode")
	       (:file "client")
	       (:file "server")))

