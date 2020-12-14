;;;; cl-notebook.asd

(asdf:defsystem #:cl-notebook
  :description "A notebook-style in-browser editor for Common Lisp"
  :author "Inaimathi <leo.zovic@gmail.com>"
  :license "AGPL3"
  :serial t
  :depends-on (#+sbcl #:sb-introspect #:qlot
	       #:alexandria #:anaphora #:cl-fad #:closer-mop
	       #:cl-who #:cl-css #:parenscript
	       #:house #:fact-base)
  :components ((:module
                src :components
                ((:file "package")
                 (:file "model")
                 (:file "util")
                 (:file "publish-update")

                 (:module
                  ui :components
                  ((:module
                    http-api :components
                    ((:file "system")
                     (:file "notebook")
                     (:file "cell")))
                   (:module
                    http-front-end :components
                    ((:file "macros")
                     (:file "css")
                     (:file "core")

                     (:file "base") (:file "templates") (:file "api")
                     (:file "notebook-selector")
                     (:file "pareditesque")))))

                 (:file "evaluators")
                 (:file "exporters")

                 (:file "main")))))

(asdf:defsystem #:cl-notebook/test
  :description "Test suite for :cl-notebook"
  :author "Inaimathi <leo.zovic@gmail.com>"
  :license "AGPL3"
  :serial t
  :depends-on (#:cl-notebook #:test-utils)
  :defsystem-depends-on (#:prove-asdf)
  :components ((:module
		test :components
		((:file "package")
		 (:test-file "cl-notebook"))))
  :perform (test-op
	    :after (op c)
	    (funcall (intern #.(string :run) :prove) c)))
