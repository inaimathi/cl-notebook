(in-package #:cl-notebook)

;;;;; Model-related
(defvar *notebooks* 
  (make-hash-table :test 'equal))

(defmethod new-cell! ((book fact-base) &key (cell-type :common-lisp))
  (multi-insert! book `((:cell nil) (:cell-type ,cell-type) (:contents "") (:value ""))))

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
  (let ((book (make-fact-base :indices '(:a :b :ab :abc) :file-name (merge-pathnames (fact-base::temp-file-name) *books*))))
    (insert-new! book :notebook-name name)
    (unless (gethash name *notebooks*)
      (setf (gethash name *notebooks*) book))
    book))

(defmethod eval-notebook-code ((book fact-base))
  (loop for cell-id in (caddar (lookup book :b :cell-order))
     when (lookup book :a cell-id :b :cell-type :c :common-lisp)
     do (js-eval :common-lisp (caddar (lookup book :a cell-id :b :contents)))))

(defmethod load-notebook! ((name pathname))
  (let ((book (load! :fact-base name :indices '(:a :b :ab :abc))))
    (eval-notebook-code book)
    (setf (gethash (notebook-name book) *notebooks*) book)))

(defun get-notebook (name)
  (gethash name *notebooks*))

;;;;; Read/eval-related
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

;;;;; HTTP Handlers
(define-json-handler (system/list-books) ()
  (alexandria:hash-table-keys *notebooks*))

(define-json-handler (notebook/rename) ((book :notebook) (new-name :string))
  (let* ((old-name (notebook-name book))
	 (book (rename-notebook! book new-name)))
    (write-delta! book)
    (publish! :updates (update :book old-name :action 'rename-book :new-name new-name))
    (current book)))

(define-json-handler (notebook/current) ((book :notebook))
  (current book))

(define-json-handler (notebook/new) ()
  (let* ((name (format nil "book-~a" (hash-table-count *notebooks*)))
	 (book (new-notebook! name)))
    (write! book)
    (publish! :updates (update :action 'new-book :book-name name))
    (current book)))

(define-json-handler (notebook/eval-to-cell) ((book :notebook) (cell-id :integer) (contents :string))
  (let* ((cont-fact (first (lookup book :a cell-id :b :contents)))
	 (val-fact (first (lookup book :a cell-id :b :value)))
	 (cell-type (caddar (lookup book :a cell-id :b :cell-type)))
	 (res (js-eval cell-type contents)))
    (when (and cont-fact val-fact res)
      (delete! book cont-fact)
      (delete! book val-fact)
      (insert! book (list cell-id :contents contents))
      (insert! book (list cell-id :value res))
      (write-delta! book)
      (publish! :updates (update :book (notebook-name book) :cell cell-id :action 'eval-to-cell :contents contents :value res))
      (current book))))

(define-json-handler (notebook/new-cell) ((book :notebook) (cell-type :keyword))
  (let ((cell-id (new-cell! book :cell-type cell-type)))
    (write-delta! book)
    (publish! :updates (update :book (notebook-name book) :action 'new-cell :cell-id cell-id :cell-type cell-type))
    (current book)))

(define-json-handler (notebook/reorder-cells) ((book :notebook) (cell-order :json))
  (awhen (lookup book :b :cell-order)
    (delete! book (car it)))
  (insert-new! book :cell-order cell-order)
  (write-delta! book)
  (publish! :updates (update :book (notebook-name book) :action 'reorder-cells :new-order cell-order))
  (alist :result :ok))

(define-json-handler (notebook/kill-cell) ((book :notebook) (cell-id :integer))
  (loop for f in (lookup book :a cell-id) do (delete! book f))
  (write-delta! book)
  (publish! :updates (update :book (notebook-name book) :cell cell-id :action 'kill-cell))
  (current book))

(define-json-handler (notebook/change-cell-type) ((book :notebook) (cell-id :integer) (new-type :keyword))
  (let ((cont-fact (first (lookup book :a cell-id :b :contents)))
	(val-fact (first (lookup book :a cell-id :b :value)))
	(tp-fact (first (lookup book :a cell-id :b :cell-type))))
    (unless (eq (third tp-fact) new-type)
      (let ((res (js-eval new-type (third cont-fact))))
	(delete! book tp-fact)
	(delete! book val-fact)
	(insert! book (list cell-id :cell-type new-type))
	(insert! book (list cell-id :value res))
	(write-delta! book)
	(publish! :updates (update :book (notebook-name book) :cell cell-id :action 'change-cell-type :new-type new-type :value res))
	(current book)))))

(define-stream-handler (source) ()
  (subscribe! :updates sock))

;;;;; System entry
(defun main (&optional argv) 
  (declare (ignore argv))
  (setf *storage* (merge-pathnames ".cl-notebook/" (user-homedir-pathname))
	*books* (merge-pathnames "books/" *storage*))
  (ensure-directories-exist *storage*)
  (ensure-directories-exist *books*)

  (in-package :cl-notebook)
  
  (dolist (book (cl-fad:list-directory *books*))
    (load-notebook! book))

  (let* ((root (asdf:system-source-directory :cl-notebook)))
    (define-file-handler (merge-pathnames "static" root) :stem-from "static"))

  (start 4242))

(defun main-dev ()
  (house::debug!)
  (bt:make-thread (lambda () (main))))
