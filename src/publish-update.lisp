(in-package #:cl-notebook)

(defun publish-update-internal! (book action k/v-pairs)
  (let ((hash (make-hash-table)))
    (loop for (k v) on k/v-pairs by #'cddr
       do (setf (gethash k hash) v))
    (setf (gethash :book hash) book
	  (gethash :action hash) action)
    (publish! :cl-notebook-updates (json:encode-json-to-string hash))
    nil))

(defmethod publish-update! (book (action symbol) &rest k/v-pairs)
  (publish-update-internal! book action k/v-pairs))

(defmethod publish-update! ((book notebook) (action symbol) &rest k/v-pairs)
  (publish-update-internal! (notebook-id book) action k/v-pairs))
