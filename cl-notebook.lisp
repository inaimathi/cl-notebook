(in-package #:cl-notebook)

(defvar *notebooks* 
  (make-hash-table :test 'equal))

(defmethod new-cell! ((book fact-base) &optional (cell-type :code))
  (multi-insert! book `((:cell nil) (:cell-type ,cell-type) (:contents "") (:value ""))))

(defmethod remove-notebook! (name)
  (remhash name *notebooks*))

(defun new-notebook! (name)
  (let ((book (make-fact-base :indices '(:a :b :c :ab) :id name)))
    (insert! (list 0 :notebook-name name) book)
    (new-cell! book)
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
      (:script :type "text/javascript" :src "/js/base.js")
      (:script :type "text/javascript" :src "/js/main.js")
      (:link :rel "stylesheet" :href "static/css/codemirror.css")
      (:link :rel "stylesheet" :href "static/codemirror-3.22/addon/dialog/dialog.css")
      (:link :rel "stylesheet" :href "static/codemirror-3.22/addon/hint/show-hint.css")
      (:script :type "text/javascript" :src "static/codemirror-3.22/lib/codemirror.js")
      (:script :type "text/javascript" :src "static/codemirror-3.22/mode/commonlisp/commonlisp.js")
      (:script :type "text/javascript" :src "static/codemirror-3.22/addon/edit/closebrackets.js")
      (:script :type "text/javascript" :src "static/codemirror-3.22/addon/edit/matchbrackets.js")
      (:script :type "text/javascript" :src "static/codemirror-3.22/addon/search/search.js")
      (:script :type "text/javascript" :src "static/codemirror-3.22/addon/search/searchcursor.js")
      (:script :type "text/javascript" :src "static/codemirror-3.22/addon/search/match-highlighter.js")
      (:script :type "text/javascript" :src "static/codemirror-3.22/addon/selection/active-line.js")
      (:script :type "text/javascript" :src "static/codemirror-3.22/addon/selection/mark-selection.js")
      (:script :type "text/javascript" :src "static/codemirror-3.22/addon/hint/show-hint.js")
      (:script :type "text/javascript" :src "static/codemirror-3.22/addon/hint/anyword-hint.js")
      (:script :type "text/javascript" :src "static/codemirror-3.22/addon/dialog/dialog.js"))
     (:body
      (:h1 "Hello!")
      (:textarea :class "cell" :style "display: none;")
      (:pre :class "result")))))

(defun single-eval-for-js (s-exp)
  (capturing-error (format nil "~a" s-exp)
    (let ((res (multiple-value-list (ignoring-warnings (eval s-exp)))))
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

(defun eval-capturing-stdout (string)
  (let* ((res nil)
	 (stdout (with-output-to-string (*standard-output*)
		   (setf res (eval-from-string string)))))
    (values res stdout)))

(defun js-eval (thing)
  (with-js-error
    (multiple-value-bind (res stdout) (eval-capturing-stdout thing)
      (alist
       :request thing 
       :result res
       :stdout stdout))))

(define-json-handler (eval) ((thing :string))
  (js-eval thing))

(defun html-tree-to-string (html-tree)
  (cadar (cl-who::tree-to-commands (list html-tree) nil)))

(defun js-whoify (thing)
  (with-js-error
    (alist
     :request thing
     :result (html-tree-to-string (read-from-string thing)))))

(define-json-handler (whoify) ((thing :string))
  (js-whoify thing))

(define-json-handler (notebook/current) ((book :notebook))
  (current book))

(define-json-handler (notebook/eval-to-cell) ((book :notebook) (cell-id :integer) (contents :string))
  (let ((cont-fact (first (lookup book :a cell-id :b :contents)))
	(val-fact (first (lookup book :a cell-id :b :value)))
	(res (js-eval contents)))
    (when (and cont-fact val-fact res)
      (delete! cont-fact book)
      (delete! val-fact book)
      (insert! (list cell-id :contents contents) book)
      (insert! (list cell-id :value res) book)
      (current book))))

(define-json-handler (notebook/new-cell) ((book :notebook) (cell-type :cell-type))
  (new-cell! book cell-type)
  (current book))

(define-json-handler (notebook/kill-cell) ((book :notebook) (cell-id :integer))
  (loop for f in (lookup book :a cell-id) do (delete! f book))
  (current book))

(defun main (&optional argv) 
  (declare (ignore argv))
  (define-file-handler "static")
  (load-notebook! "test-book")
  (start 4242))
