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
	      (with-output-to-string (s) 
		(let ((*standard-output* s))
		  (setf res (multiple-value-list (eval (read-from-string thing))))))))
	(hash () 
	  :request thing 
	  :result-type (mapcar #'type-of res) 
	  :result (mapcar (lambda (a) (format nil "~a" a)) res)
	  :output stdio))
    (error (e)
      (hash ()
	:request thing
	:result-type (list "error")
	:result (list (cl-mop:to-alist e))))))

(define-closing-handler (js/base.js :content-type "application/javascrip") ()
  (ps (defun dom-ready (callback)
	(chain document (add-event-listener "DOMContentLoaded" callback)))

      (defun by-selector (selector)
	(chain document (query-selector selector)))

      (defun dom-append (elem markup)
	(let ((new-content (chain document (create-element "div"))))
	  (setf (@ new-content inner-h-t-m-l) markup)
	  (loop while (@ new-content first-child)
	     do (chain elem (append-child (@ new-content first-child))))))

      (defun escape (string)
	(chain string
	       (replace "<" "&lt;")
	       (replace ">" "&gt;")))

      (defun dom-set (elem markup)
	(setf (@ elem inner-h-t-m-l) markup))

      (defun by-selector* (selector)
	(chain document (query-selector-all selector)))

      (defun number? (obj) (string= "number" (typeof obj)))
      (defun string? (obj) (string= "string" (typeof obj)))
      (defun function? (obj) (string= "function" (typeof obj)))

      (defun type? (obj type-string)
	(eql (chain -object prototype to-string (call obj)) type-string))
      (defun object? (obj) (type? obj "[object Object]"))

      (defun join (strings &optional (separator "")) (chain strings (join separator)))

      (defun encode (string)
	(encode-u-r-i-component string))
     
      (defun string->obj (string)
	(chain -j-s-o-n (parse string)))

      (defun obj->string (object)
	(chain -j-s-o-n (stringify object)))

      (defun obj->params (object)
	(let ((res (new (-array))))
	  (for-in (k object) 
		  (let ((val (aref object k)))
		    (chain res (push (+ (encode k) "=" (encode (if (object? val) (obj->string val) val)))))))
	  (chain res (join "&"))))

      (defun get (uri params callback)
	(let ((req (new (-x-m-l-http-request))))
	  (setf (@ req onreadystatechange)
		(lambda ()
		  (when (and (equal (@ req ready-state) 4)
			     (equal (@ req status) 200))
		    (let ((result (@ req response-text)))
		      (callback result)))))
	  (chain req (open :GET (if params (+ uri "?" (obj->params params)) uri) t))
	  (chain req (send))))

      (defun post (uri params callback)
	(let ((req (new (-x-m-l-http-request)))
	      (encoded-params (obj->params params)))
	  (setf (@ req onreadystatechange)
		(lambda ()
		  (when (and (equal (@ req ready-state) 4)
			     (equal (@ req status) 200))
		    (let ((result (@ req response-text)))
		      (callback result)))))
	  (chain req (open :POST uri t))
	  (chain req (set-request-header "Content-type" "application/x-www-form-urlencoded"))
	  (chain req (set-request-header "Content-length" (length encoded-params)))
	  (chain req (set-request-header "Connection" "close"))
	  (chain req (send encoded-params))))))

(define-closing-handler (js/main.js :content-type "application/javascript") ()
  (ps (defun server/eval (thing target-elem)
	(post "/eval" (create :thing thing)
	      (lambda (raw) 
		(let ((res (string->obj raw)))
		  (chain console (log raw))
		  (dom-set (by-selector ".result") 
			   (join (loop for r in (@ res :result)
				    for out = (if (object? r) (obj->string r) (escape r))
				    for tp in (@ res "resultType")
				    collect (who-ps-html (:p out (:span :class "type" " :: " tp))))))))))

      (defvar *mirror* nil)
      
      (dom-ready 
       (lambda ()
	 (let ((repl (by-selector ".cell"))
	       (options (create "lineNumbers" t
				"matchBrackets" t
				"autoCloseBrackets" t
				"extraKeys" (create "Ctrl-Enter" (lambda (cmd) (server/eval (chain *mirror* (get-value)) (by-selector ".result")))))))
	   (setf *mirror* (chain -code-mirror (from-text-area repl options))))))))

(defvar *server* (bt:make-thread (lambda () (start 4242))))
