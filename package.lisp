;;;; package.lisp
(defpackage #:cl-notebook
  (:use #:cl #:house #:parenscript #:cl-who #:fact-base)
  (:import-from #:anaphora #:aif #:awhen #:it)
  (:import-from #:alexandria #:with-gensyms)
  (:shadowing-import-from 
   #+openmcl-native-threads #:ccl 
   #+cmu #:pcl 
   #+sbcl #:sb-pcl 
   #+lispworks #:hcl 
   #+allegro #:mop 
   #+clisp #:clos 
   #:class-slots #:slot-definition-name)
  (:shadowing-import-from #:fact-base #:lookup))

(in-package #:cl-notebook)

(defvar *storage*)
(defvar *books*)

(define-http-type (:notebook)
    :type-expression `(gethash ,parameter *notebooks*)
    :lookup-assertion `(typep ,parameter 'fact-base))

(define-http-type (:cell-type)
    :type-expression `(intern (string-upcase ,parameter) :keyword)
    :lookup-assertion `(member ,parameter '(:common-lisp :cl-who)))
