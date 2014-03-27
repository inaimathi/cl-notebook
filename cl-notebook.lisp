(in-package #:cl-notebook)

(defvar *notebooks* 
  (make-hash-table :test 'equal))

(defmethod new-cell! ((book fact-base) &key (cell-type :common-lisp) (contents "") (value ""))
  (multi-insert! book `((:cell nil) (:cell-type ,cell-type) (:contents ,contents) (:value ,value))))

(defmethod remove-notebook! (name)
  (remhash name *notebooks*))

(defun new-notebook! (name)
  (let ((book (make-fact-base :indices '(:a :b :c :ab) :file-name name)))
    (insert-new! book :notebook-name name)
    (let ((cont (format nil "(:h1 \"~a\")" name)))
      (new-cell! book :cell-type :cl-who :contents cont :value (js-whoify cont)))
    (unless (gethash name *notebooks*)
      (setf (gethash name *notebooks*) book))))

(defun load-notebook! (name)
  (setf (gethash name *notebooks*) (load! name :indices '(:a :b :c :ab))))

(defun get-notebook (name)
  (gethash name *notebooks*))

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
      (:script :type "text/javascript" :src "/static/js/addons/dialog.js"))

     (:body))))

(defun single-eval-for-js (s-exp)
  (capturing-error (format nil "~s" s-exp)
    (let ((res (multiple-value-list (ignoring-warnings (eval s-exp)))))
      (loop for v in res collect (list (type-label v) (format nil "~s" v))))))

(defun read-all-from-string (str)
  (let ((len (length str))
	(start 0))
    (loop for (s-exp next) = (multiple-value-list (read-from-string str nil nil :start start))
       do (setf start next)
       collect s-exp
       until (or (not (numberp next)) (= len next)))))

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

(defun js-eval (thing)
  (with-js-error
    (multiple-value-bind (res stdout) (capturing-stdout (eval-from-string thing))
      (alist
       :result res
       :stdout stdout))))

(define-json-handler (eval) ((thing :string))
  (js-eval thing))

(defun html-tree-to-string (html-tree)
  (cadar (cl-who::tree-to-commands 
	  (if (keywordp (car html-tree))
	      (list html-tree)
	      html-tree) nil)))

(defun js-whoify (thing)
  (with-js-error
    (alist :result (html-tree-to-string (read-all-from-string thing)))))

(define-json-handler (whoify) ((thing :string))
  (js-whoify thing))

(define-json-handler (notebook/current) ((book :notebook))
  (current book))

(defmethod js-ev (cell-type (contents string))
  (alist :result contents :stdout ""))

(defmethod js-ev ((cell-type (eql :cl-who)) (contents string))
  (js-whoify contents))

(defmethod js-ev ((cell-type (eql :common-lisp)) (contents string))
  (js-eval contents))

(define-json-handler (notebook/eval-to-cell) ((book :notebook) (cell-id :integer) (contents :string))
  (let* ((cont-fact (first (lookup book :a cell-id :b :contents)))
	 (val-fact (first (lookup book :a cell-id :b :value)))
	 (cell-type (caddar (lookup book :a cell-id :b :cell-type)))
	 (res (js-ev cell-type contents)))
    (when (and cont-fact val-fact res)
      (delete! book cont-fact)
      (delete! book val-fact)
      (insert! book (list cell-id :contents contents))
      (insert! book (list cell-id :value res))
      (write-delta! book)
      (current book))))

(define-json-handler (notebook/new-cell) ((book :notebook) (cell-type :cell-type))
  (new-cell! book :cell-type cell-type)
  (write-delta! book)
  (current book))

(define-json-handler (notebook/reorder-cells) ((book :notebook) (cell-order :json))
  (awhen (lookup book :b :cell-order)
    (delete! book (car it)))
  (insert-new! book :cell-order cell-order)
  (write-delta! book)
  (alist :result :ok))

(define-json-handler (notebook/kill-cell) ((book :notebook) (cell-id :integer))
  (loop for f in (lookup book :a cell-id) do (delete! book f))
  (write-delta! book)
  (current book))

(defun main (&optional argv) 
  (declare (ignore argv))
  (let* ((root (asdf:system-source-directory :cl-notebook)))
    (define-file-handler (merge-pathnames "static" root) :stem-from "static"))
  (load-notebook! "test-book")
  (start 4242))
