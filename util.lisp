(in-package :cl-notebook)

(defun update (&rest k/v-pairs)
  (json:encode-json-to-string (apply #'alist k/v-pairs)))

(defun instance->alist (instance)
  (loop for s in (closer-mop:class-slots (class-of instance))
     for slot-name = (slot-definition-name s)
     when (slot-boundp instance slot-name)
     collect (cons (intern (symbol-name slot-name) :keyword) 
		   (slot-value instance slot-name))))

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
  (let ((err-alist (instance->alist e)))
    `(error 
      ((error-type . ,(type-of e))
       ,@(let ((f-tmp (cdr (assoc :format-control err-alist)))
	       (f-args (cdr (assoc :format-arguments err-alist))))
	      (when (and f-tmp f-args)
		(list (cons :error-message (apply #'format nil f-tmp f-args)))))
       ,@(when form
	       (list (cons :form form)))
       ,@(remove-if #'ignored-error-prop? err-alist)))))

(defmacro ignore-redefinition-warning (&body body)
  `(locally
       (declare #+sbcl(sb-ext:muffle-conditions sb-kernel:redefinition-warning))
     (handler-bind
	 (#+sbcl(sb-kernel:redefinition-warning #'muffle-warning))
       ,@body)))

(defmacro capturing-stdout (&body body)
  (with-gensyms (res stdout)
    `(let* ((,res nil)
	    (,stdout (with-output-to-string (*standard-output*)
		       (setf ,res (progn ,@body)))))
       (values ,res ,stdout))))

(defmacro capturing-error (form &body body)
  `(handler-case
       (ignore-redefinition-warning ,@body)
     (t (e) (values (list (front-end-error ,form e)) :error))))

(defmacro with-js-error (&body body)
  `(handler-case
       (ignore-redefinition-warning ,@body)
     (t (e) (alist :result (list (list (front-end-error nil e))) :stdout ""))))

