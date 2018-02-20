;;;; cl-notebook.asd

(asdf:defsystem #:cl-notebook
  :serial t
  :description "A notebook-style in-browser editor for Common Lisp"
  :author "Inaimathi <leo.zovic@gmail.com>"
  :license "AGPL3"
  :depends-on (#+sbcl #:sb-introspect
	       #:alexandria #:anaphora #:cl-fad #:closer-mop
	       #:cl-who #:cl-css #:parenscript
	       #:house #:fact-base)
  :components ((:module
                src :components
                ((:file "package")
                 (:file "model")
                 (:file "util")

                 (:module
                  front-end :components
                  ((:file "macros")
                   (:file "css")
                   (:file "core")))

                 (:file "evaluators")
                 (:file "cl-notebook")

                 (:file "start")))))
