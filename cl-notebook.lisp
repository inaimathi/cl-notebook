(in-package #:cl-notebook)

(define-file-handler "static")

(define-closing-handler (root) ()
  (with-html-output-to-string (s nil :prologue t :indent t)
    (:html
     (:head
      (:script :type "text/javascript" :src "/js/base.js")
      (:script :type "text/javascript" :src "/js/main.js")
      (:link :rel "stylesheet" :href "static/codemirror-3.22/lib/codemirror.css")
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

(defun eval-from-string (str)
  (let ((len (length str))
	(start 0))
    (loop for (s-exp next) = (multiple-value-list (capturing-error (read-from-string str nil nil :start start)))
       do (setf start next)

       if (typep s-exp 'error) collect (list 'error s-exp) into res-list and do (return res-list)
       else collect (let ((res (multiple-value-list (eval s-exp))))
		      (loop for v in res collect (list (type-label v) (format nil "~s" v)))) into res-list 
       
       when (and next (= len next)) do (return (car (last res-list))))))

(defun eval-capturing-stdout (string)
  (let* ((res nil)
	 (stdio (with-output-to-string (*standard-output*)
		  (setf res (eval-from-string string)))))
    (values res stdio)))

;; TODO - figure out why this explodes on read errors
(define-json-handler (eval) (thing)
  (with-js-error
    (multiple-value-bind (res stdio) (eval-capturing-stdout thing)
	(hash ()
	  :request thing 
	  :result res
	  :output stdio))))

(defun html-tree-to-string (html-tree)
  (cadar (cl-who::tree-to-commands (list html-tree) nil)))

(define-json-handler (whoify) (thing)
  (with-js-error
    (hash ()
      :request thing
      :result (html-tree-to-string (read-from-string thing)))))

(defvar *server* (bt:make-thread (lambda () (start 4242))))
