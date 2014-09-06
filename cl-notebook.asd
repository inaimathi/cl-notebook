;;;; cl-notebook.asd

(asdf:defsystem #:cl-notebook
  :serial t
  :description "A notebook-style in-browser editor for Common Lisp"
  :author "Inaimathi <leo.zovic@gmail.com>"
  :license "AGPL3"
  :depends-on (#:alexandria #:anaphora #:house #:fact-base #:parenscript #:cl-fad #:cl-who #:cl-css #:closer-mop)
  :components ((:file "package")
	       (:file "util")
	       (:file "front-end-macros")
	       (:file "css")
	       (:file "front-end")
	       (:file "model")
               (:file "cl-notebook")
	       (:file "charts")
	       (:file "start")))

