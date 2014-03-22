(in-package #:cl-notebook)

(define-file-handler "static")

(define-closing-handler (root) ()
  (with-html-output-to-string (s nil :prologue t :indent t)
    (:html
     (:head
      (:script :type "text/javascript" :src "/js/base.js")
      (:script :type "text/javascript" :src "/js/main.js")
      (:link :rel "stylesheet" :href "static/css/codemirror.css")
      (:link :rel "stylesheet" :href "static/codemirror-3.22/addon/dialog/dialog.css")
      (:script :type "text/javascript" :src "static/codemirror-3.22/lib/codemirror.js")
      (:script :type "text/javascript" :src "static/codemirror-3.22/mode/commonlisp/commonlisp.js")
      (:script :type "text/javascript" :src "static/codemirror-3.22/addon/edit/closebrackets.js")
      (:script :type "text/javascript" :src "static/codemirror-3.22/addon/edit/matchbrackets.js")
      (:script :type "text/javascript" :src "static/codemirror-3.22/addon/search/search.js")
      (:script :type "text/javascript" :src "static/codemirror-3.22/addon/search/searchcursor.js")
      (:script :type "text/javascript" :src "static/codemirror-3.22/addon/search/match-highlighter.js")
      (:script :type "text/javascript" :src "static/codemirror-3.22/addon/selection/active-line.js")
      (:script :type "text/javascript" :src "static/codemirror-3.22/addon/selection/mark-selection.js")
      (:script :type "text/javascript" :src "static/codemirror-3.22/addon/dialog/dialog.js"))
     (:body
      (:h1 "Hello!")
      (:textarea :class "cell" :style "display: none;")
      (:pre :class "result")))))

(defun eval-for-js (s-exp)
  (capturing-error (format nil "~a" s-exp)
    (let ((res (multiple-value-list (ignoring-warnings (eval s-exp)))))
      (loop for v in res collect (list (type-label v) (format nil "~s" v))))))

(defun eval-from-string (str)
  (let ((len (length str))
	(start 0))
    (loop for (s-exp next) = (multiple-value-list (capturing-error nil (read-from-string str nil nil :start start)))
       for (evaled eval-error?) = (unless (eq next :error) (multiple-value-list (eval-for-js s-exp)))
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

(define-json-handler (eval) ((thing :string))
  (with-js-error
    (multiple-value-bind (res stdout) (eval-capturing-stdout thing)
      (hash ()
	:request thing 
	:result res
	:stdout stdout))))

(defun html-tree-to-string (html-tree)
  (cadar (cl-who::tree-to-commands (list html-tree) nil)))

(define-json-handler (whoify) (thing)
  (with-js-error
    (hash ()
      :request thing
      :result (html-tree-to-string (read-from-string thing)))))

(defun main (&optional argv) 
  (declare (ignore argv))
  (start 4242))

;; (defvar *server* (bt:make-thread (lambda () (start 4242))))

