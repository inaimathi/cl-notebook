(in-package #:cl-notebook)

; Basic server-side definitions and handlers
;;;;; Read/eval-related
;; (defmethod front-end-eval (cell-language cell-type (contents string))
;;   "A cell that fits no other description is self-evaluating"
;;   (list
;;    (alist 
;;     :stdout "" :warnings nil ; Without these, :json encodes this as an array rather than an object
;;     :values (list (alist :type "string" :value contents)))))

;; (defmethod front-end-eval ((cell-language (eql :common-lisp)) cell-type (contents string))
;;   "A Common-Lisp:Code cell is just evaluated, capturing all warnings, stdout emissions and errors."
;;   (capturing-eval contents))

;; (defmethod front-end-eval ((cell-language (eql :common-lisp)) (cell-type (eql :markup)) (contents string))
;;   "A Common-Lisp:Markup cell is evaluated as a :cl-who tree"
;;   (list
;;    (alist :stdout "" :warnings nil ; Without these, :json encodes this as an array rather than an object
;; 	  :values
;; 	  (handler-case
;; 	      (list
;; 	       (alist 
;; 		:type "string"
;; 		:value (eval 
;; 			`(with-html-output-to-string (s) 
;; 			   ,@(read-all contents)))))
;; 	    (error (e)
;; 	      (list (alist :type 'error :value (front-end-error nil e))))))))




(defmethod js-eval (cell-type (contents string))
  (list 
   (alist 
    :stdout "" :warnings nil 
    :values (list (alist :type "string" :value contents)))))

(defmethod js-eval ((cell-type (eql :cl-who)) (contents string))
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

(defmethod js-eval ((cell-type (eql :common-lisp)) (contents string))
  (capturing-eval contents))

;;;;; Model-related
(defvar *notebooks* 
  (make-hash-table :test 'equal))
(defvar *front-end-eval-thread* nil)

(defmethod new-cell! ((book fact-base) &key (cell-type :common-lisp))
  (multi-insert! book `((:cell nil) (:cell-type ,cell-type) (:contents "") (:result ""))))

(defmethod remove-notebook! (name)
  (remhash name *notebooks*))

(defmethod notebook-name ((book fact-base))
  (caddar (lookup book :b :notebook-name)))

(defmethod rename-notebook! ((book fact-base) (new-name string))
  (let ((name-fact (first (lookup book :b :notebook-name))))
    (delete! book name-fact)
    (insert-new! book :notebook-name new-name)
    (setf (gethash new-name *notebooks*) book)
    (remhash (third name-fact) *notebooks*)
    book))

(defun new-notebook! (name)
  (let ((book (make-fact-base :indices *default-indices* :file-name (merge-pathnames (fact-base::temp-file-name) *books*))))
    (insert-new! book :notebook-name name)
    (unless (gethash name *notebooks*)
      (setf (gethash name *notebooks*) book))
    book))

(defmethod eval-notebook-code ((book fact-base))
  (loop for cell-id in (caddar (lookup book :b :cell-order))
     when (lookup book :a cell-id :b :cell-type :c :common-lisp)
     do (js-eval :common-lisp (caddar (lookup book :a cell-id :b :contents)))))

(defmethod make-unique-name-in ((dir pathname) (base-name string))
  (assert (cl-fad:directory-pathname-p dir))
  (let ((name (merge-pathnames base-name dir)))
    (if (cl-fad:file-exists-p name)
	(loop for i from 0
	   for name = (merge-pathnames (format nil "~a-~a" base-name i) dir)
	   unless (cl-fad:file-exists-p name) do (return name))
	name)))

(defmethod kill! ((book fact-base))
  (let ((trash-name (make-unique-name-in *trash*  (file-namestring (file-name book)))))
    (rename-file (file-name book) trash-name)
    (remhash (notebook-name book) *notebooks*)))

(defmethod load-notebook! ((name pathname))
  (let ((book (load! name :indices *default-indices*)))
    (eval-notebook-code book)
    (setf (gethash (notebook-name book) *notebooks*) book)))

(defun get-notebook (name)
  (gethash name *notebooks*))

;;;;; HTTP Handlers
(define-json-handler (cl-notebook/system/list-books) ()
  (alexandria:hash-table-keys *notebooks*))

(define-json-handler (cl-notebook/system/kill-thread) ()
  (when (and (bt:threadp *front-end-eval-thread*)
	     (bt:thread-alive-p *front-end-eval-thread*))
    (bt:destroy-thread *front-end-eval-thread*)
    (publish! :cl-notebook-updates (update :action 'killed-eval)))
  :ok)

(define-json-handler (cl-notebook/notebook/current) ((book :notebook))
  (current book))

(define-json-handler (cl-notebook/notebook/new) ()
  (let* ((name (format nil "book-~a" (hash-table-count *notebooks*)))
	 (book (new-notebook! name)))
    (write! book)
    (publish! :cl-notebook-updates (update :action 'new-book :book-name name))
    (current book)))

(define-json-handler (cl-notebook/notebook/kill) ((book :notebook))
  (kill! book)
  (publish! :cl-notebook-updates (update :book (notebook-name book) :action 'kill-book))
  :ok)

(define-json-handler (cl-notebook/notebook/rename) ((book :notebook) (new-name :string))
  (let* ((old-name (notebook-name book))
	 (book (rename-notebook! book new-name)))
    (write-delta! book)
    (publish! :cl-notebook-updates (update :book old-name :action 'rename-book :new-name new-name)))
  :ok)

(define-json-handler (cl-notebook/notebook/eval-to-cell) ((book :notebook) (cell-id :integer) (contents :string))
  (let ((cont-fact (first (lookup book :a cell-id :b :contents)))
	(val-fact (first (lookup book :a cell-id :b :result)))
	(cell-type (caddar (lookup book :a cell-id :b :cell-type))))
    (delete! book cont-fact)
    (insert! book (list cell-id :contents contents))
    (publish! :cl-notebook-updates (update :book (notebook-name book) :cell cell-id :action 'content-changed :contents contents))
    (when (and (bt:threadp *front-end-eval-thread*)
	       (bt:thread-alive-p *front-end-eval-thread*))
      (bt:destroy-thread *front-end-eval-thread*))
    (publish! :cl-notebook-updates (update :book (notebook-name book) :cell cell-id :action 'starting-eval))
    (setf *front-end-eval-thread*
	  (bt:make-thread
	   (lambda ()
	     (let ((res (js-eval cell-type contents)))
	       (when (and cont-fact val-fact res)
		 (delete! book val-fact)
		 (insert! book (list cell-id :result res))
		 (write-delta! book)
		 (publish! :cl-notebook-updates (update :book (notebook-name book) :cell cell-id :action 'finished-eval :contents contents :result res))))))))
  :ok)

(define-json-handler (cl-notebook/notebook/new-cell) ((book :notebook) (cell-type :keyword))
  (let ((cell-id (new-cell! book :cell-type cell-type)))
    (write-delta! book)
    (publish! :cl-notebook-updates (update :book (notebook-name book) :action 'new-cell :cell-id cell-id :cell-type cell-type)))
  :ok)

(define-json-handler (cl-notebook/notebook/reorder-cells) ((book :notebook) (cell-order :json))
  (awhen (lookup book :b :cell-order)
    (delete! book (car it)))
  (insert-new! book :cell-order cell-order)
  (write-delta! book)
  (publish! :cl-notebook-updates (update :book (notebook-name book) :action 'reorder-cells :new-order cell-order))
  :ok)

(define-json-handler (cl-notebook/notebook/kill-cell) ((book :notebook) (cell-id :integer))
  (loop for f in (lookup book :a cell-id) do (delete! book f))
  (write-delta! book)
  (publish! :cl-notebook-updates (update :book (notebook-name book) :cell cell-id :action 'kill-cell))
  :ok)

(define-json-handler (cl-notebook/notebook/change-cell-type) ((book :notebook) (cell-id :integer) (new-type :keyword))
  (let ((cont-fact (first (lookup book :a cell-id :b :contents)))
	(val-fact (first (lookup book :a cell-id :b :result)))
	(tp-fact (first (lookup book :a cell-id :b :cell-type))))
    (unless (eq (third tp-fact) new-type)
      (let ((res (js-eval new-type (third cont-fact))))
	(delete! book tp-fact)
	(delete! book val-fact)
	(insert! book (list cell-id :cell-type new-type))
	(insert! book (list cell-id :result res))
	(write-delta! book)
	(publish! :cl-notebook-updates (update :book (notebook-name book) :cell cell-id :action 'change-cell-type :new-type new-type :result res)))))
  :ok)

(define-json-handler (cl-notebook/notebook/change-cell-noise) ((book :notebook) (cell-id :integer) (new-noise :keyword))
  (awhen (first (lookup book :a cell-id :b :noise))
    (delete! book it))
  (unless (eq new-noise :normal)
    (insert! book (list cell-id :noise new-noise)))
  (publish! :cl-notebook-updates (update :book (notebook-name book) :cell cell-id :action 'change-cell-noise :new-noise new-noise))
  :ok)

(define-stream-handler (cl-notebook/source) ()
  (subscribe! :cl-notebook-updates sock))
