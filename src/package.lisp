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
  (:shadowing-import-from #:fact-base #:lookup)
  (:export :bar-graph :draw-bar-graph :main :str :htm :create :@ :chain :define-js :define-css))

(in-package #:cl-notebook)

(defvar *storage* nil)
(defvar *books* nil)
(defvar *static* nil)

(defvar *static-files* nil)
(defvar *quicklisp-file* nil)

(defvar *default-indices* '(:a :b :ab :abc))

(define-http-type (:notebook)
    :type-expression `(get-notebook! ,parameter)
    :type-assertion  `(typep ,parameter 'notebook))

(define-http-type (:existing-filepath)
    :type-expression `(pathname ,parameter)
    :type-assertion `(cl-fad:file-exists-p ,parameter))

(define-http-type (:existing-directory)
    :type-expression `(pathname ,parameter)
    :type-assertion `(cl-fad:directory-exists-p ,parameter))

(define-http-type (:nonexistent-file)
    :type-expression `(pathname ,parameter)
    :type-assertion `(not (or (cl-fad:directory-exists-p ,parameter)
                              (cl-fad:file-exists-p ,parameter))))
