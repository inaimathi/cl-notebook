(in-package :cl-notebook)

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
	  (chain req (send encoded-params))))

      (defun post/json (uri params callback)
	(post uri params
	      (lambda (raw)
		(let ((res (string->obj raw)))
		  (callback res)))))))

(define-closing-handler (js/main.js :content-type "application/javascript") ()
  (ps 
    (defun result-template (result)
      (+ (if (@ result :output) (who-ps-html (:p :class "stdout" (@ result :output))) "")
	 (join
	  (loop for form-res in (@ result :result)
	     append (who-ps-html
		     (:div :class "result-set"
			   (join
			    (loop for (tp val) in form-res
			       if (= tp :error) collect (who-ps-html (:p :class "error" (obj->string val)))
			       else collect (who-ps-html (:p val " :: " tp))))))))))

    (defun server/eval (thing target-elem)
      (post/json "/eval" (create :thing thing)
	    (lambda (res) 
	      (dom-set target-elem (result-template res)))))

    (defun server/whoify (thing target-elem)
      (post/json "/whoify" (create :thing thing)
		 (lambda (res)
		   (chain console (log "WHOIFIED" res))
		   (dom-set target-elem (@ res :result)))))

    (defun editor-keys (content-thunk)
      (create "Ctrl-Enter" 
	      (lambda (cmd)		
		(server/eval 
		 (content-thunk)
		 (by-selector ".result")))
	      "Ctrl-Space"
	      (lambda (cmd)
		(chain console (log "CTRL-SPACE!")))))
    
    (defun mirror! (target-textarea)
      (let* ((mirror)
	     (options (create 
		       "lineNumbers" t 
		       "matchBrackets" t
		       "autoCloseBrackets" t
		       "viewportMargin" -infinity
		       "extraKeys" (editor-keys (lambda () (chain mirror (get-value)))))))
	(setf mirror (chain -code-mirror (from-text-area target-textarea options)))
	mirror))
    
    (dom-ready 
     (lambda ()
       (mirror! (by-selector ".cell"))))))
