;;;; package.lisp
(defpackage #:cl-notebook
  (:use #:cl #:house #:parenscript #:cl-who #:fact-base)
  (:import-from #:anaphora #:aif #:awhen #:it)
  (:import-from #:alexandria #:with-gensyms)
  (:shadowing-import-from #:fact-base #:lookup))

