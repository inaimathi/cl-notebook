(in-package #:cl-notebook)

; Basic server-side definitions and handlers
;;;;; Read/eval-related
(defvar *front-end-eval-thread* nil)
(defmethod front-end-eval (cell-language cell-type (contents string))
  "A cell that fits no other description returns a"
  (list
   (alist 
    :stdout "" :warnings nil ; Without these, :json encodes this as an array rather than an object
    :values (list (alist :type 'error :value 
			 (alist 'condition-type "UNKNOWN-LANGUAGE:TYPE-COMBINATION" 
				'cell-language cell-language
				'cell-type cell-type))))))

(defmethod front-end-eval (cell-language cell-type (contents null)) "")

(defmethod front-end-eval ((cell-language (eql :common-lisp)) cell-type (contents string))
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

(defmethod eval-notebook ((book notebook) &optional (cell-type :code))
  (let ((ids (notebook-cell-order book)))
    (loop for cell-id in ids
       when (and (lookup book :a cell-id :b :cell-language :c :common-lisp)
		 (lookup book :a cell-id :b :cell-type :c cell-type))
       do (let ((stale? (first (lookup book :a cell-id :b :stale :c t)))
		(res-fact (first (lookup book :a cell-id :b :result)))
		(*package* (namespace book)))
	    (let ((res 
		   (handler-case
		       (bt:with-timeout (.1)
			 (front-end-eval 
			  :common-lisp cell-type 
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
		   (write! book)
		   (publish-update! book 'finished-package-eval :contents contents)))
	     (error (e)
	       (publish-update! book 'finished-package-eval :contents contents :result (front-end-error nil e))))))))

;;;;; HTTP Handlers
(define-json-handler (cl-notebook/system/kill-thread) ()
  (when (and (bt:threadp *front-end-eval-thread*)
	     (bt:thread-alive-p *front-end-eval-thread*))
    (bt:destroy-thread *front-end-eval-thread*))
  (publish-update! nil 'killed-eval)
  :ok)

(define-json-handler (cl-notebook/system/complete) ((partial :string) (package :keyword))
  (let ((p (string-upcase partial))
	(res))
    (do-symbols (s package)
      (when (alexandria:starts-with-subseq p (symbol-name s))
	(push s res)))
    (sort (mapcar (lambda (s) (string-downcase (symbol-name s)))
		  (remove-duplicates res))
	  #'< :key #'length)))

(define-handler (cl-notebook/system/macroexpand-1 :content-type "plain/text") ((expression :string))
  (format nil "~s" (macroexpand-1 (read-from-string expression))))

(define-handler (cl-notebook/system/macroexpand :content-type "plain/text") ((expression :string))
  (format nil "~s" (macroexpand (read-from-string expression))))

(defmethod arglist ((fn symbol))
  #+ccl (ccl:arglist fn)
  #+lispworks (lw:function-lambda-list fn)
  #+clisp (or (ignore-errors
		(second (function-lambda-expression fn)))
	      (ext:arglist fn))
  #+sbcl(sb-introspect:function-lambda-list fn))

(define-json-handler (cl-notebook/system/arg-hint) ((name :string) (package :keyword))
  (multiple-value-bind (sym-name fresh?) (intern (string-upcase name) package)
    (if (fboundp sym-name)
	(hash :args (labels ((->names (thing)
			       (cond ((and (listp thing) (eql (car thing) '&environment))
				      (->names (cddr thing)))
				     ((listp thing)
				      (mapcar #'->names thing))
				     (t
				      (string-downcase (symbol-name thing))))))
		      (->names (arglist sym-name))))
	(progn (when (not fresh?) (unintern sym-name))
	       (hash :error :function-not-found)))))

(define-json-handler (cl-notebook/notebook/rewind) ((book :notebook) (index :integer))
  (hash :facts (rewind-to book index) :history-size (total-entries book) :history-position index :id (notebook-id book)))

(define-json-handler (cl-notebook/notebook/current) ((book :notebook))
  (hash :facts (current book) :history-size (total-entries book) :id (notebook-id book)))

(define-json-handler (cl-notebook/notebook/fork-at) ((book :notebook) (index :integer))
  (let ((new (load! (fork-at book index :file-name (merge-pathnames (fact-base::temp-file-name) *books*))
		    :indices *default-indices* :in-memory? t))
	(new-name (format nil "Fork of ~a" (notebook-name book))))
    (rename-notebook! new new-name)
    (register-notebook! new)
    (publish-update! new 'new-book :book-name new-name)
    (hash :facts (current new) :history-size (total-entries new) :id (notebook-id new) :book-name new-name)))

(define-json-handler (cl-notebook/notebook/new) ()
  (let* ((name (format nil "book-~a" (hash-table-count *notebooks*)))
	 (book (new-notebook! name)))
    (write! book)
    (publish-update! book 'new-book :book-name name)
    (hash :facts (current book) :history-size (total-entries book) :id (notebook-id book))))

(define-json-handler (cl-notebook/notebook/kill) ((book :notebook))
  (kill! book)
  (publish-update! book 'kill-book)
  :ok)

(define-json-handler (cl-notebook/notebook/repackage) ((book :notebook) (new-package :string))
  (unless (string= new-package (notebook-package-spec-string book))
    (eval-package book new-package))
  :ok)

(define-json-handler (cl-notebook/notebook/rename) ((book :notebook) (new-name :string))
  (multiple-value-bind (book renamed?) (rename-notebook! book new-name)
    (when renamed? 
      (unless (or (lookup book :b :package-edited?) (lookup book :b :package-error))
	(repackage-notebook! book (default-package book))
	(publish-update! book 'finished-package-eval :contents (notebook-package-spec-string book)))
      (write! book)
      (publish-update! book 'rename-book :new-name new-name)))
  :ok)

(define-json-handler (cl-notebook/notebook/eval-to-cell) ((book :notebook) (cell-id :integer) (contents :string))
  (let ((cont-fact (first (lookup book :a cell-id :b :contents)))
	(val-fact (first (lookup book :a cell-id :b :result)))
	(cell-lang (caddar (lookup book :a cell-id :b :cell-language)))
	(cell-type (caddar (lookup book :a cell-id :b :cell-type))))
    (change! book cont-fact (list cell-id :contents contents))
    (publish-update! book 'content-changed :cell cell-id :contents contents)
    (eval-cell book cell-id contents val-fact cell-lang cell-type))
  :ok)

(define-json-handler (cl-notebook/notebook/change-cell-contents) ((book :notebook) (cell-id :integer) (contents :string))
  (let ((cont-fact (first (lookup book :a cell-id :b :contents))))
    (unless (string= contents (third cont-fact))
      (change! book cont-fact (list cell-id :contents contents))
      (insert! book (list cell-id :stale t))
      (publish-update! book 'content-changed :cell cell-id :contents contents)))
  :ok)

(define-json-handler (cl-notebook/notebook/change-cell-language) ((book :notebook) (cell-id :integer) (new-language :keyword))
  (let ((cont-fact (first (lookup book :a cell-id :b :contents)))
	(val-fact (first (lookup book :a cell-id :b :result)))	
	(cell-type (caddar (lookup book :a cell-id :b :cell-type)))
	(lang-fact (first (lookup book :a cell-id :b :cell-language))))
    (unless (eq (third lang-fact) new-language)
      (change! book lang-fact (list cell-id :cell-type new-language))
      (publish-update! book 'change-cell-language :cell cell-id :new-language new-language)
      (eval-cell book cell-id (third cont-fact) val-fact new-language cell-type)))
  :ok)

(define-json-handler (cl-notebook/notebook/change-cell-type) ((book :notebook) (cell-id :integer) (new-type :keyword))
  (let ((cont-fact (first (lookup book :a cell-id :b :contents)))
	(val-fact (first (lookup book :a cell-id :b :result)))
	(cell-lang (caddar (lookup book :a cell-id :b :cell-language)))
	(tp-fact (first (lookup book :a cell-id :b :cell-type))))
    (unless (eq (third tp-fact) new-type)
      (change! book tp-fact (list cell-id :cell-type new-type))
      (publish-update! book 'change-cell-type :cell cell-id :new-type new-type)
      (eval-cell book cell-id (third cont-fact) val-fact cell-lang new-type)))
  :ok)

(define-json-handler (cl-notebook/notebook/new-cell) ((book :notebook) (cell-language :keyword) (cell-type :keyword))
  (let ((cell-id (new-cell! book :cell-type cell-type :cell-language cell-language)))
    (write! book)
    (publish-update! book 'new-cell :cell-id cell-id :cell-type cell-type :cell-language cell-language))
  :ok)

(define-json-handler (cl-notebook/notebook/reorder-cells) ((book :notebook) (cell-order :json))
  (awhen (first (lookup book :b :cell-order))
    (delete! book it))
  (insert-new! book :cell-order cell-order)
  (write! book)
  (publish-update! book 'reorder-cells :new-order cell-order)
  :ok)

(define-json-handler (cl-notebook/notebook/kill-cell) ((book :notebook) (cell-id :integer))
  (loop for f in (lookup book :a cell-id) do (delete! book f))
  (write! book)
  (publish-update! book 'kill-cell :cell cell-id)
  :ok)

(define-json-handler (cl-notebook/notebook/change-cell-noise) ((book :notebook) (cell-id :integer) (new-noise :keyword))
  (let ((old-noise-fact (first (lookup book :a cell-id :b :noise)))
	(new-noise-fact (unless (eq new-noise :normal) (list cell-id :noise new-noise))))
    (cond ((and old-noise-fact new-noise-fact)
	   (change! book old-noise-fact new-noise-fact))
	  (old-noise-fact
	   (delete! book old-noise-fact))
	  (new-noise-fact
	   (insert! book new-noise-fact))))
  (publish-update! book 'change-cell-noise :cell cell-id :new-noise new-noise)
  :ok)

(define-handler (cl-notebook/source :close-socket? nil) ()
  (subscribe! :cl-notebook-updates sock))
