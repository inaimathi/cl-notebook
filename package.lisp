;;;; package.lisp
(defpackage #:cl-notebook
  (:use #:cl #:house #:parenscript #:cl-who #:fact-base)
  (:import-from #:anaphora #:aif #:awhen #:it)
  (:import-from #:alexandria #:with-gensyms)
  (:shadowing-import-from #:fact-base #:lookup))

(in-package #:cl-notebook)

(define-http-type (:notebook)
    :type-expression `(gethash ,parameter *notebooks*)
    :lookup-assertion `(typep ,parameter 'fact-base))

(define-http-type (:cell-type)
    :type-expression `(intern (string-upcase ,parameter) :keyword)
    :lookup-assertion `(member ,parameter '(:code :markup)))
