(in-package :cl-notebook)

(defmethod ->keyword ((sym symbol))
  (intern (symbol-name sym) :keyword))

(defun type-label (thing)
  (let ((tp (type-of thing)))
    (if (listp tp)
	(first tp)
	tp)))

(defun alist (&rest k/v-pairs)
  (loop for (k v) on k/v-pairs by #'cddr
     collect (cons k v)))

(defun ignored-error-prop? (pair)
  (member (first pair) 
	  (list :args :control-string :second-relative :print-banner
		:references :format-control :format-arguments :offset
		:stream)))

(defun front-end-error (form e)
  (let ((err-alist (loop for (a . b) in (cl-mop:to-alist e) 
		      collect (cons (->keyword a) b))))
    `((error 
       ((error-type . ,(type-of e))
	,@(let ((f-tmp (cdr (assoc :format-control err-alist)))
		(f-args (cdr (assoc :format-arguments err-alist))))
	       (when (and f-tmp f-args)
		 (list (cons :error-message (apply #'format nil f-tmp f-args)))))
	,@(when form
		(list (cons :form form)))
	,@(remove-if #'ignored-error-prop? err-alist))))))

(defmacro ignoring-warnings (&body body)
  `(locally
       (declare #+sbcl(sb-ext:muffle-conditions sb-kernel:redefinition-warning))
     (handler-bind
	 (#+sbcl(sb-kernel:redefinition-warning #'muffle-warning))
       ,@body)))

(defmacro capturing-error (form &body body)
  `(handler-case
       (ignoring-warnings ,@body)
     (error (e) (values (front-end-error ,form e) :error))))

(defmacro capturing-stdout (&body body)
  (with-gensyms (res stdout)
    `(let* ((,res nil)
	    (,stdout (with-output-to-string (*standard-output*)
		       (setf ,res (progn ,@body)))))
       (values ,res ,stdout))))

(defmacro with-js-error (&body body)
  `(handler-case
       (ignoring-warnings ,@body)
     (error (e) (alist :result (front-end-error nil e) :stdout ""))))

