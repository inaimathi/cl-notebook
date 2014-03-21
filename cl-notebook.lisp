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

(define-json-handler (eval) (thing)
  (handler-case
      (let* ((res nil)
	     (stdio 
	      (with-output-to-string (*standard-output*)
		(setf res (multiple-value-list (eval (read-from-string thing)))))))
	(hash () 
	  :request thing 
	  :result-type (mapcar #'type-of res) 
	  :result (mapcar (lambda (a) (format nil "~a" a)) res)
	  :output stdio))
    (error (e)
      (hash ()
	:request thing
	:result-type "error"
	:result (cl-mop:to-alist e)))))

(defun html-tree-to-string (html-tree)
  (cadar (cl-who::tree-to-commands (list html-tree) nil)))

(define-json-handler (whoify) (thing)
  (handler-case
      (hash ()
	:request thing
	:result-type "text/html"
	:result (html-tree-to-string (read-from-string thing)))
    (error (e)
      (hash ()
	:request thing
	:result-type "error"
	:result (cl-mop:to-alist e)))))

(defvar *server* (bt:make-thread (lambda () (start 4242))))
