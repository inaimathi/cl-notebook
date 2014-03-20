(in-package #:cl-notebook)

(define-closing-handler (root) ()
  (with-html-output-to-string (s nil :prologue t :indent t)
    (:html
     (:head
      (:script :type "text/javascript" :src "/js/base.js")
      (:script :type "text/javascript" :src "/js/main.js"))
     (:body
      (:h1 "Hello!")
      (:input)))))

(define-json-handler (eval) (thing)
  (let ((res (eval (read-from-string thing))))
    (hash () 
      :request thing 
      :result-type (type-of res) 
      :result res)))

(define-closing-handler (js/base.js :content-type "application/javascrip") ()
  (ps (defun dom-ready (callback)
	(chain document (add-event-listener "DOMContentLoaded" callback)))

      (defun by-selector (selector)
	(chain document (query-selector selector)))

      (defun by-selector* (selector)
	(chain document (query-selector-all selector)))

      (defun number? (obj) (string= "number" (typeof obj)))
      (defun string? (obj) (string= "string" (typeof obj)))
      (defun function? (obj) (string= "function" (typeof obj)))

      (defun type? (obj type-string)
	(eql (chain -object prototype to-string (call obj)) type-string))
      (defun object? (obj) (type? obj "[object Object]"))

      (defun encode (string)
	(encode-u-r-i-component string))

      (defun obj->string (object)
	(chain -j-s-o-n-s (stringify object)))

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
	 (chain (by-selector "input") 
		(add-event-listener 
		 :keydown (lambda (event)
			    (when (= 13 (@ event key-code))
			      (post "/eval" (create :thing (@ (by-selector "input") value))
				    (lambda (res)
				      (chain console (log res))))))))))))

(defvar *server* (bt:make-thread (lambda () (start 4242))))
