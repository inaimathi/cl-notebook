(in-package :cl-notebook)

(defmacro hash ((&key (test 'eql)) &body k/v-pairs)
  (with-gensyms (tbl)
    `(let ((,tbl (make-hash-table :test ',test)))
       (setf ,@(loop for (k v) on k/v-pairs by #'cddr
		  collect `(gethash ,k ,tbl) 
		  collect v))
       ,tbl)))
