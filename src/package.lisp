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
  (:export :bar-graph :draw-bar-graph :main :str :htm :who-ps-html :new :create :@ :chain :define-css))

(in-package #:cl-notebook)

(defvar *storage* nil)
(defvar *books* nil)
(defvar *static* nil)
(defvar *ql* nil)

(defvar *static-files* nil)

(defvar *default-indices* '(:a :b :ab :abc))

(defun >>notebook (book-id)
  (let ((book (get-notebook! book-id)))
    (assert (typep book 'notebook))
    book))

(defun >>package (name)
  (let ((pkg (or (find-package name) (find-package (intern (string-upcase name) :keyword)))))
    (assert (not (null pkg)))
    pkg))

(defun >>existing-filepath (path)
  (let ((p (pathname path)))
    (assert (cl-fad:file-exists-p p))
    p))

(defun >>existing-directory (path)
  (let ((p (pathname path)))
    (assert (cl-fad:directory-exists-p p))
    p))

(defun >>nonexistent-file (path)
  (let ((p (pathname path)))
    (not (or (cl-fad:directory-exists-p p)
             (cl-fad:file-exists-p p)))
    p))

(defun >>export-format (format)
  (let ((fmt (intern (string-upcase format) :keyword)))
    (member fmt (export-book-formats))
    fmt))
