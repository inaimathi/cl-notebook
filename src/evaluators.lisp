(in-package #:cl-notebook)

(defvar *front-end-eval-thread* nil)
(defmethod front-end-eval (cell-language cell-type (contents string))
  "A cell that fits no other description returns an error"
  (list
   (alist
    :stdout "" :warnings nil ; Without these, :json encodes this as an array rather than an object
    :values (list (alist :type 'error :value
			 (alist 'condition-type "UNKNOWN-LANGUAGE:TYPE-COMBINATION"
				'cell-language cell-language
				'cell-type cell-type))))))

(defmethod front-end-eval (cell-language cell-type (contents null)) "")

(defmethod front-end-eval ((cell-language (eql :common-lisp)) (cell-type (eql :tests)) (contents string))
  "A Common-Lisp:Test cell is just evaluated, capturing all warnings, stdout emissions and errors.
It's treated differently in export situations."
  (capturing-eval contents))

(defmethod front-end-eval ((cell-language (eql :common-lisp)) (cell-type (eql :code)) (contents string))
  "A Common-Lisp:Code cell is just evaluated, capturing all warnings, stdout emissions and errors."
  (capturing-eval contents))

(defmethod front-end-eval ((cell-language (eql :common-lisp)) (cell-type (eql :markup)) (contents string))
  "A Common-Lisp:Markup cell is evaluated as a :cl-who tree"
  (list
   (alist :stdout "" :warnings nil ; Without these, :json encodes this as an array rather than an object
	  :values
	  (handler-case
	      (list
	       (alist
		:type "string"
		:value (eval
			`(with-html-output-to-string (s)
			   ,@(read-all contents)))))
	    (error (e)
	      (list (alist :type 'error :value (front-end-error nil e))))))))

(defmethod front-end-eval ((cell-language (eql :common-lisp)) (cell-type (eql :parenscript)) (contents string))
  "A Common-Lisp:Parenscript cell is evaluated as a `ps` form"
  (list
   (alist :stdout "" :warnings nil
          :values
          (handler-case
	      (list
	       (alist
		:type "js"
		:value (apply #'ps* (read-all contents))))
	    (error (e)
	      (list (alist :type 'error :value (front-end-error nil e))))))))

(defun front-end-eval-formats ()
  (let ((h (make-hash-table)))
    (loop for m in (closer-mop:generic-function-methods #'front-end-eval)
       for (a b _) = (closer-mop:method-specializers m)
       when (and (typep a 'closer-mop:eql-specializer) (typep b 'closer-mop:eql-specializer))
       do (push (closer-mop:eql-specializer-object b)
                (gethash (closer-mop:eql-specializer-object a) h nil)))
    h))

(defmethod eval-notebook ((book notebook) &key (cell-type :code))
  (let ((ids (notebook-cell-order book)))
    (loop for cell-id in ids
       when (lookup book :a cell-id :b :cell-type :c cell-type)
       do (let ((stale? (first (lookup book :a cell-id :b :stale :c t)))
		(res-fact (first (lookup book :a cell-id :b :result)))
		(*package* (namespace book)))
	    (let ((res
		   (handler-case
		       (bt:with-timeout (.1)
			 (front-end-eval
			  (caddar (lookup book :a cell-id :b :cell-language))
                          cell-type
			  (caddar (lookup book :a cell-id :b :contents))))
		     #-sbcl (bordeaux-threads:timeout () :timed-out)
		     #+sbcl (sb-ext:timeout () :timed-out))))
	      (unless (eq :timed-out res)
		(when stale? (delete! book stale?))
		(unless (equalp (third res-fact) res)
		  (let ((new (list cell-id :result res)))
		    (if res-fact
			(change! book res-fact new)
			(insert! book new))))))))
    (write! book)))

(defmethod empty-expression? ((contents string))
  (when (cl-ppcre:scan "^[ \n\t\r]*$" contents) t))

(defmethod eval-cell ((book notebook) cell-id (contents string) res-fact cell-language cell-type)
  (unless (empty-expression? contents)
    (when (and (bt:threadp *front-end-eval-thread*)
	       (bt:thread-alive-p *front-end-eval-thread*))
      (bt:destroy-thread *front-end-eval-thread*))
    (publish-update! book 'starting-eval :target cell-id)
    (setf *front-end-eval-thread*
	  (bt:make-thread
	   (lambda ()
	     (let ((*package* (namespace book)))
	       (let ((res (front-end-eval cell-language cell-type contents)))
		 (when (and res-fact res)
		   (change! book res-fact (list cell-id :result res))
		   (delete! book (list cell-id :stale t))
		   (write! book))
		 (publish-update! book 'finished-eval :cell cell-id :contents contents :result res))))))))

(defmethod eval-package ((book notebook) (contents string))
  (when (and (bt:threadp *front-end-eval-thread*)
	     (bt:thread-alive-p *front-end-eval-thread*))
    (bt:destroy-thread *front-end-eval-thread*))
  (publish-update! book 'starting-eval :target :package)
  (setf *front-end-eval-thread*
	(bt:make-thread
	 (lambda ()
	   (handler-case
	       (multiple-value-bind (book repackaged?) (repackage-notebook! book contents)
		 (when repackaged?
		   (unless (lookup book :b :package-edited?)
		     (insert-new! book :package-edited? t))
		   (write! book))
		 (publish-update! book 'finished-package-eval :contents contents))
	     (error (e)
	       (publish-update! book 'finished-package-eval :contents contents :result (front-end-error nil e))))))))
