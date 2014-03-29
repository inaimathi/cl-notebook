;;;; cl-notebook.asd

(asdf:defsystem #:cl-notebook
  :serial t
  :description "Describe cl-notebook here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria #:anaphora #:house #:fact-base #:parenscript #:cl-who #:cl-css)
  :components ((:file "package")
	       (:file "util")
	       (:file "front-end")
               (:file "cl-notebook")))

