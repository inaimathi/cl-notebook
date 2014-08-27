;;;; package.lisp
(defpackage #:cl-notebook
  (:use #:cl #:house #:parenscript #:cl-who #:fact-base)
  (:import-from #:cl-css #:inline-css)
  (:shadowing-import-from #:cl-css #:%)  
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

(defvar *storage* nil)
(defvar *books* nil)
(defvar *trash* nil)
(defvar *static* nil)

(defvar *static-files* nil)

(defvar *default-indices* '(:a :b :ab :abc))

(define-http-type (:notebook)
    :type-expression `(get-notebook ,parameter)
    :type-assertion  `(typep ,parameter 'fact-base))
