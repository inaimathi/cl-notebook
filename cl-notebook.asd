;;;; cl-notebook.asd

(asdf:defsystem #:cl-notebook
  :serial t
  :description "Describe cl-notebook here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria #:anaphora #:house #:fact-base #:parenscript #:cl-fad #:cl-who #:cl-css #:closer-mop)
  :components ((:file "package")
	       (:file "util")
	       (:file "front-end-macros")
	       (:file "css")
	       (:file "front-end")
               (:file "cl-notebook")))

