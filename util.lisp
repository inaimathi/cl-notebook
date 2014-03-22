(in-package :cl-notebook)

(defmethod ->keyword ((sym symbol))
  (intern (symbol-name sym) :keyword))

(defun type-label (thing)
  (let ((tp (type-of thing)))
    (if (listp tp)
	(first tp)
	tp)))

(defmacro hash ((&key (test 'eql)) &body k/v-pairs)
  (with-gensyms (tbl)
    `(let ((,tbl (make-hash-table :test ',test)))
       (setf ,@(loop for (k v) on k/v-pairs by #'cddr
		  collect `(gethash ,k ,tbl) 
		  collect v))
       ,tbl)))

(defmacro capturing-error (&body body)
  `(handler-case
       (progn ,@body)
     (error (e) e)))

(defun ignored-error-prop? (pair)
  (member (->keyword (first pair)) 
	  (list :args :control-string :second-relative :print-banner
		:references :format-control :format-arguments)))

(defmacro with-js-error (&body body)
  `(handler-case
       (progn ,@body)
     (error (e)
       (hash ()
	 :result (list (list :error
			     (cons (cons 'error-type (type-of e))
				   (remove-if #'ignored-error-prop?
					      (cl-mop:to-alist e)))))))))

