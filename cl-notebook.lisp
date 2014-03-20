(in-package #:cl-notebook)

(define-closing-handler (root) ()
  (with-html-output-to-string (s nil :prologue t :indent t)
    (:html
     (:head
      (:script :type "text/javascript" :src "/js/base.js")
      (:script :type "text/javascript" :src "/js/main.js"))
     (:body
      (:h1 "Hello!")
      (:textarea :class "cell")
      (:pre :class "result")))))

(define-json-handler (eval) (thing)
  (handler-case
      (let ((res (multiple-value-list (eval (read-from-string thing)))))
	(hash () 
	  :request thing 
	  :result-type (mapcar #'type-of res) 
	  :result res))
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

(define-closing-handler (js/main.js :content-type "application/javascrip") ()
  (ps (dom-ready 
       (lambda ()
	 (chain console (log "something!"))
	 (let ((repl (by-selector ".cell")))
	   (chain repl (add-event-listener 
			:keydown (lambda (event)
				   (when (and (@ event ctrl-key) (= 13 (@ event key-code)))
				     (post "/eval" (create :thing (@ this value))
					   (lambda (raw) 
					     (let ((res (string->obj raw)))
					       (chain console (log res))
					       (dom-set (by-selector ".result") 
							(join 
							 (loop for r in (@ res :result)
							    for out = (if (object? r) (obj->string r) r)
							    for tp in (@ res "resultType")
							    collect (who-ps-html (:p out (:span :class "type" " :: " tp))))))))))))))))))

(defvar *server* (bt:make-thread (lambda () (start 4242))))
