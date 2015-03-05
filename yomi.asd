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
	       :cl-json
	       :cl-ppcre)
  :serial t
  :components ((:file "packages")
	       (:file "parameters")

	       (:file "plot")
	       (:file "code")
	       (:file "client")
	       (:file "server")))

