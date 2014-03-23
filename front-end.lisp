(in-package :cl-notebook)

(define-closing-handler (js/base.js :content-type "application/javascript") ()
  (ps (defun dom-ready (callback)
	(chain document (add-event-listener "DOMContentLoaded" callback)))

      (defun identity (thing) thing)

      (defun map (fn thing)
	(if (object? thing)
	    (let ((res (-array)))
	      (for-in (k thing) (chain res (push (fn (aref thing k) k))))
	      res)
	    (loop for elem in thing collect (fn elem))))

      (defun vals (obj) (map identity obj))
      
      (defun by-selector (selector)
	(chain document (query-selector selector)))

      (defun get-page-hash ()
	(let ((hash (@ window location hash))
	      (res (create)))
	  (when hash
	    (loop for pair in (chain (=rest hash) (split "&"))
	       for (k v) = (chain pair (split "="))
	       do (setf (aref res (decode-u-r-i k)) (decode-u-r-i v)))
	    res)))
	   
      (defun set-page-hash (hash-object)
	(setf (@ window location hash) (obj->params hash-object)))

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
	(join 
	 (map (lambda (v k) 
		(+ (encode k) "=" 
		   (encode (if (object? v) (obj->string v) v))))
	      object)
	 "&"))

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
      (when result
	(+ (if (@ result :stdout) (who-ps-html (:p :class "stdout" (@ result :stdout))) "")
	   (join
	    (loop for form-res in (@ result :result)
	       append (who-ps-html
		       (:div :class "result-set"
			     (join
			      (loop for (tp val) in form-res
				 if (= tp :error) collect (who-ps-html (:p :class "error" (obj->string val)))
				 else collect (who-ps-html (:p val " :: " tp)))))))))))

    (defun cell-template (cell)
      (with-slots (id contents value) cell
	(who-ps-html 
	 (:div :class "cell" :id (+ "cell-" id)
	       (:button :onclick (+ "killCell(" id ")") "-")
	       (:textarea :class "cell-contents" contents)
	       (:pre :class "cell-value" (result-template value))))))
    
    (defun notebook-template (notebook)
      (+ (who-ps-html 
	  (:h3 (notebook-name notebook))
	  (:button :onclick "newCell()" "+")
	  (:p))
	 (join (map (lambda (cell) (cell-template cell)) (notebook-cells notebook)))))
    
    (defun server/eval (thing target-elem)
      (console.log "SERVER/EVAL" thing target-elem)
      (post/json "/eval" (create :thing thing)
	    (lambda (res) 
	      (console.log "SERVER/EVAL-CALLBACK" thing res)
	      (dom-set target-elem (result-template res)))))

    (defun server/whoify (thing target-elem)
      (post/json "/whoify" (create :thing thing)
		 (lambda (res)
		   (console.log "WHOIFIED" res)
		   (dom-set target-elem (@ res :result)))))

    (defun server/notebook/current (name callback)
      (post/json "/notebook/current" (create :book name)
		 (lambda (res)
		   (console.log "SHOWING BOOK" res)
		   (callback res))))

    (defun server/notebook/eval-to-cell (cell-id contents)
      (post/json "/notebook/eval-to-cell" (create :book (notebook-name *notebook*) :cell-id cell-id :contents contents)
		 #'notebook!))
    
    (defun mirror! (cell-id)
      (let* ((mirror)
	     (options (create 
		       "lineNumbers" t 
		       "matchBrackets" t
		       "autoCloseBrackets" t
		       "viewportMargin" -infinity
		       "extraKeys" 
		       (create "Ctrl-Enter"
			       (lambda (cmd)
				 (server/notebook/eval-to-cell 
				  cell-id (chain mirror (get-value))))
			       "Ctrl-Space" "autocomplete"))))
	(setf 
	 mirror 
	 (chain -code-mirror 
		(from-text-area 
		 (by-selector (+ "#cell-" cell-id " .cell-contents"))
		 options)))
	mirror))
    
    (defvar *notebook*)

    (defun notebook-condense (notebook)
      (let ((res (create)))
	(loop for (id prop val) in notebook
	   unless (aref res id) do (setf (aref res id) (create :id id))
	   do (if (null val)
		  (setf (aref res id :type) prop)
		  (setf (aref res id prop) val)))
	res))

    (defun notebook-name (notebook) (@ notebook :name))

    (defun notebook-facts (notebook) (@ notebook :facts))
    (defun notebook-objects (notebook) (@ notebook :objects))
    (defun notebook-cell-ordering (notebook)
      (loop for (a b c) in (notebook-facts notebook)
	 when (equal b :cell-order) do (return c)
	 when (and (equal b :cell) (null c)) collect a into implicit-ord
	 finally (return (chain implicit-ord (reverse)))))
    (defun notebook-cells (notebook)
      (let ((ord (notebook-cell-ordering notebook))
	    (obj (notebook-objects notebook)))
	(map (lambda (id) (aref obj id)) ord)))

    (defun notebook! (raw)
      (let ((book (notebook-condense raw)))
	(setf *notebook* 
	      (create :facts raw
		      :objects book
		      :name (loop for (a b c) in raw
			       when (equal b "notebookName")
			       do (return c))))
	(dom-set (by-selector "body") (notebook-template *notebook*))
	(map (lambda (cell) (mirror! (@ cell :id))) (notebook-cells *notebook*))))

    (defun new-cell ()
      (post/json "/notebook/new-cell" (create :book (notebook-name *notebook*) :cell-type :code)
		 #'notebook!))
    
    (defun kill-cell (cell-id)
      (post/json "/notebook/kill-cell" (create :book (notebook-name *notebook*) :cell-id cell-id)
		 #'notebook!))

    (dom-ready 
     (lambda ()
       (server/notebook/current "test-book" #'notebook!)))))
