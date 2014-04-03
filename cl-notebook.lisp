(in-package #:cl-notebook)

;;;;; Model-related
(defvar *notebooks* 
  (make-hash-table :test 'equal))

(defmethod new-cell! ((book fact-base) &key (cell-type :common-lisp) (contents "") (value ""))
  (multi-insert! book `((:cell nil) (:cell-type ,cell-type) (:contents ,contents) (:value ,value))))

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
(defun read-all-from-string (str)
  (let ((len (length str))
	(start 0))
    (loop for (s-exp next) = (multiple-value-list (read-from-string str nil nil :start start))
       do (setf start next)
       collect s-exp
       until (or (not (numberp next)) (= len next)))))

(defun single-eval-for-js (s-exp)
  (capturing-error (format nil "~s" s-exp)
    (let ((res (multiple-value-list (ignore-redefinition-warning (eval s-exp)))))
      (loop for v in res collect (list (type-label v) (format nil "~s" v))))))

(defun eval-from-string (str)
  (let ((len (length str))
	(start 0))
    (loop for (s-exp next) = (multiple-value-list (capturing-error nil (read-from-string str nil nil :start start)))
       for (evaled eval-error?) = (unless (eq next :error) (multiple-value-list (single-eval-for-js s-exp)))
       if (eq next :error) collect s-exp into res-list
       else collect evaled into res-list
	 
       do (setf start next)
       when (or (eq next :error) (eq eval-error? :error)) do (return res-list)
       when (and next (numberp next) (= len next)) do (return (last res-list)))))

(defmethod js-eval (cell-type (contents string))
  (alist :result contents :stdout ""))

(defmethod js-eval ((cell-type (eql :cl-who)) (contents string))
  (with-js-error
    (alist :result 
	   (eval 
	    `(with-html-output-to-string (s) 
	       ,@(read-all-from-string contents)))
	   :stdout "")))

(defmethod js-eval ((cell-type (eql :common-lisp)) (contents string))
  (with-js-error
    (multiple-value-bind (res stdout) (capturing-stdout (eval-from-string contents))
      (alist
       :result res
       :stdout stdout))))

;;;;; HTTP Handlers
(define-closing-handler (root) ()
  (with-html-output-to-string (s nil :prologue t :indent t)
    (:html
     (:head
      (:title "cl-notebook")
      
      (:link :rel "stylesheet" :href "/css/notebook.css")
      (:link :rel "stylesheet" :href "/static/css/genericons.css")
      (:link :rel "stylesheet" :href "/static/css/codemirror.css")
      (:link :rel "stylesheet" :href "/static/css/dialog.css")
      (:link :rel "stylesheet" :href "/static/css/show-hint.css")

      (:script :type "text/javascript" :src "/js/base.js")
      (:script :type "text/javascript" :src "/js/main.js")
      (:script :type "text/javascript" :src "/static/js/native-sortable.js")

      (:script :type "text/javascript" :src "/static/js/codemirror.js")
      (:script :type "text/javascript" :src "/static/js/modes/commonlisp.js")
      (:script :type "text/javascript" :src "/static/js/addons/closebrackets.js")
      (:script :type "text/javascript" :src "/static/js/addons/matchbrackets.js")
      (:script :type "text/javascript" :src "/static/js/addons/search.js")
      (:script :type "text/javascript" :src "/static/js/addons/searchcursor.js")
      (:script :type "text/javascript" :src "/static/js/addons/match-highlighter.js")
      (:script :type "text/javascript" :src "/static/js/addons/active-line.js")
      (:script :type "text/javascript" :src "/static/js/addons/mark-selection.js")
      (:script :type "text/javascript" :src "/static/js/addons/show-hint.js")
      (:script :type "text/javascript" :src "/static/js/addons/anyword-hint.js")
      (:script :type "text/javascript" :src "/static/js/addons/dialog.js")
      (:script :type "text/javascript" :src "/static/js/addons/runmode/runmode.js"))

     (:body))))

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
      (publish! :updates (update :book (notebook-name book) :cell cell-id :action 'eval-to-cell :contents contents :result res))
      (current book))))

(define-json-handler (notebook/new-cell) ((book :notebook) (cell-type :keyword))
  (new-cell! book :cell-type cell-type)
  (write-delta! book)
  (publish! :updates (update :book (notebook-name book) :action 'new-cell :cell-type cell-type))
  (current book))

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
	(publish! :updates (update :book (notebook-name book) :cell cell-id :action 'change-cell-type :new-type new-type))
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
  
  (dolist (book (cl-fad:list-directory *books*)) (load-notebook! book))

  (in-package :cl-notebook)
  (let* ((root (asdf:system-source-directory :cl-notebook)))
    (define-file-handler (merge-pathnames "static" root) :stem-from "static"))

  (start 4242))
