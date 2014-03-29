(in-package :cl-notebook)

(define-closing-handler (css/notebook.css :content-type "text/css") ()
  (cl-css:css
   `((body :font-family sans-serif)

     ("button" 
      :height 24px :min-width 34px
      :font-size x-large 
      :border "2px solid #ccc" :border-radius 4px :cursor pointer 
      :float left :margin-right 5px :color "#666")
     ("button .btn-text"
      :font-size medium :display inline-block)
     ("button:hover"
      :color "#000")

     (select
      :height 24px :font-weight bolder :color "#666" 
      :border "2px solid #ccc" :border-radius 4px)
     ("select:hover"
      :color "#000" :background-color "#eee")

     (.main-controls 
      :z-index 5 :position fixed :top -20px
      :width 60% :left 50% :margin-left -30%)
     (".main-controls:hover" :top 2px)
     (".main-controls button"
      :height 32px)
     
     (.cells :list-style-type none :padding 0px :margin 0px :margin-top 38px)
     (".cells .cell" :padding 5px :margin-bottom 10px :border-top "3px solid transparent" :background-color "#fff")
     (".cells .cell.code" :background-color "#eee")

     (".cell .controls"
      :display none :position absolute :margin-top -41px :padding 5px
      :background-color "#eee" :border "2px solid #ccc" :border-bottom none :border-radius "5px 5px 0px 0px")
     (".cell .controls button" :width 32px)
     (".cell .controls span"
      :height 19px :width 31px :font-size x-large :float left :margin-right 5px :color "#666"
      :padding-top 5px :padding-left 3px :cursor move)
     (".cell .controls span:hover" :color "#000")

     (".cell:hover" :border-top "3px solid #ccc" :z-index 10)
     (".cell:hover .controls" :display block)

     (.result :border "1px solid #ccc" :background-color "#fff" :list-style-type none :margin 0px :margin-top 5px :padding 0px)
     (.stdout :margin 0px :padding 5px :color "#8b2252" :background-color "#efefef")
     (".result li" :padding 5px)
     (".result .type" :color "#228b22")
     (".result .error" :background-color "#fdd" :color "#933")

     (.error-contents :list-style-type none :margin 0px :padding 0px)
     (".error-contents .error-type" :font-weight bolder)
     (".error-contents .error-property" :font-style oblique)
     (".error-contents .error-property .label" :display inline-block :margin-right 5px :font-size small))))

(define-closing-handler (js/base.js :content-type "application/javascript") ()
  (ps 
    ;; basic functional stuff
    (defun identity (thing) thing)

    (defun rest (array) (chain array (slice 1)))

    (defun map (fn thing)
      (if (object? thing)
	  (let ((res (-array)))
	    (for-in (k thing) (chain res (push (fn (aref thing k) k))))
	    res)
	  (loop for elem in thing collect (fn elem))))

    (defun fold (fn memo thing)
      (let ((m memo))
	(if (object? thing)
	    (for-in (k thing) (setf m (fn (aref thing k) m)))
	    (loop for elem in thing do (setf m (fn elem m))))
	m))

    (defun filter (fn thing)	
      (loop for elem in thing when (fn elem) collect elem))

    (defun extend (obj &rest other-objs)
      (flet ((ext! (dest src) (map (lambda (v k) (setf (aref dest k) v)) src)))
	(let ((res (create)))
	  (ext! res obj)
	  (map (lambda (obj) (ext! res obj)) other-objs)
	  res)))

    (defun append-new (list-a list-b)
      (let ((s (new (-set list-a))))
	(chain 
	 list-a 
	 (concat
	  (loop for elem in list-b
	     unless (chain s (has elem))
	     collect elem)))))

    (defun join (strings &optional (separator "")) (chain strings (join separator)))

    ;; basic hash stuff
    (defun vals (obj) (map identity obj))

    ;; basic DOM/event stuff
    (defun prevent (ev) (chain ev (prevent-default)))

    (defun dom-ready (callback)
      (chain document (add-event-listener "DOMContentLoaded" callback)))

    (defun by-selector (selector)
      (chain document (query-selector selector)))
    (defun by-selector-all (selector)
      (chain document (query-selector-all selector)))

    (defun escape (string)
      (chain string
	     (replace "<" "&lt;")
	     (replace ">" "&gt;")))

    (defun dom-append (elem markup)
      (let ((new-content (chain document (create-element "div"))))
	(setf (@ new-content inner-h-t-m-l) markup)
	(loop while (@ new-content first-child)
	   do (chain elem (append-child (@ new-content first-child))))))

    (defun dom-set (elem markup)
      (setf (@ elem inner-h-t-m-l) markup))

    ;; basic type stuff
    (defun number? (obj) (string= "number" (typeof obj)))
    (defun string? (obj) (string= "string" (typeof obj)))
    (defun function? (obj) (string= "function" (typeof obj)))

    (defun type? (obj type-string)
      (eql (chain -object prototype to-string (call obj)) type-string))
    (defun object? (obj) (type? obj "[object Object]"))

    ;; basic encoding/decoding stuff
    (defun encode (string)
      (encode-u-r-i-component string))

    (defun decode (string)
      (decode-u-r-i string))
    
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

    ;; basic AJAX stuff
    (defun get-page-hash ()
      (let ((hash (@ window location hash))
	    (res (create)))
	(when hash
	  (loop for pair in (chain (rest hash) (split "&"))
	     for (k v) = (chain pair (split "="))
	     do (setf (aref res (decode k)) (decode v)))
	  res)))
    
    (defun set-page-hash (hash-object)
      (setf (@ window location hash) (obj->params hash-object)))

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
		    (when (function? callback)
		      (callback result))))))
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
    ;; DOM templates
    (defun error-template (err)
      (who-ps-html
       (:ul :class "error-contents"
	    (:li :class "error-type" (@ err "errorType"))
	    (:li :class "error-form" (@ err "form"))
	    (join (map 
		   (lambda (v k) 
		     (if (not (or (= k "errorType") (= k "form")))
			 (who-ps-html 
			  (:li :class "error-property" 
			       (:span :class "label" k ":") v))
			 ""))
		   err)))))

    (defun result-template (result)
      (when result ;; yes, seriously. New cells don't have these
	(who-ps-html
	 (:pre
	  (+ (if (@ result :stdout) (who-ps-html (:p :class "stdout" (@ result :stdout))) "")
	     (join (loop for form-res in (@ result :result)
		      append (who-ps-html
			      (:ul :class "result"
				   (join (loop for (tp val) in form-res
					    if (= tp :error) collect (who-ps-html (:li :class "error" (error-template val)))
					    else collect (who-ps-html (:li (:span :class "value" (escape val))
									   (:span :class "type" " :: " tp))))))))))))))

    (defun cell-controls-template (cell)
      (who-ps-html
       (:div :class "controls" 
	     (:span :class "genericon genericon-draggable")
	     (:button :class "genericon genericon-trash" 
		      :onclick (+ "killCell(" (@ cell :id) ")") "  ")
	     (:select 
	      :onchange (+ "changeCellType(" (@ cell :id) ", this.value)")
	      (join (loop for tp in (list :cl-who :common-lisp) 
		       for lb in (list 'cl-who 'common-lisp) ;; curse these symbol case issues
		       if (= (@ cell "cellType") lb)
		         collect (who-ps-html (:option :value tp :selected "selected" tp))
		       else 
			 collect (who-ps-html (:option :value tp tp))))))))

    (defun cell-markup-template (cell)
      (with-slots (id contents value language) cell
	(who-ps-html 
	 (:li :class "cell markup" :id (+ "cell-" id) :cell-id id 
	      :ondragend "reorderCells(event)"
	      (cell-controls-template cell)
	      (:textarea :class "cell-contents" :language (or language "commonlisp") contents)
	      (:div 
	       :onclick (+ "showEditor(" id ")")
	       (cond ((and (string? (@ value :result)) (= "" (@ value :result)))
		      (who-ps-html (:p (:b "[[EMPTY CELL]]"))))
		     ((string? (@ value :result))
		      (@ value :result))
		     (t (result-template value))))))))
    
    (defun cell-code-template (cell)
      (with-slots (id contents value language) cell
	(who-ps-html 
	 (:li :class "cell code" :id (+ "cell-" id) :cell-id id :ondragend "reorderCells(event)"
	      (cell-controls-template cell)
	      (:textarea :class "cell-contents" :language (or language "commonlisp")  contents)
	      (result-template value)))))

    (defun cell-template (cell)
      (case (@ cell cell-type)
	("commonLisp" (cell-code-template cell))
	("clWho" (cell-markup-template cell))))
    
    (defun notebook-template (notebook)
      (who-ps-html 
       (:div :class "main-controls"
	     (:button 
	      :onclick "newCell()"
	      (:span :class "genericon genericon-document")
	      (:span :class "btn-text" "New Cell")))
       (:ul :class "cells"
	    (join (map (lambda (cell) (cell-template cell))
		       (notebook-cells notebook))))))

    ;; AJAX calls
    (defun server/eval (thing target-elem)
      (post/json "/eval" (create :thing thing)
	    (lambda (res) (dom-set target-elem (result-template res)))))

    (defun server/whoify (thing target-elem)
      (post/json "/whoify" (create :thing thing)
		 (lambda (res) (dom-set target-elem (@ res :result)))))

    (defun server/notebook/current (name callback)
      (post/json "/notebook/current" (create :book name)
		 (lambda (res) (callback res))))

    (defun server/notebook/eval-to-cell (cell-id contents)
      (post/json "/notebook/eval-to-cell" (create :book (notebook-name *notebook*) :cell-id cell-id :contents contents)
		 #'notebook!))

    (defun new-cell (&optional (cell-type :common-lisp))
      (post/json "/notebook/new-cell" (create :book (notebook-name *notebook*) :cell-type cell-type)
		 #'notebook!))
    
    (defun kill-cell (cell-id)
      (post/json "/notebook/kill-cell" (create :book (notebook-name *notebook*) :cell-id cell-id)
		 #'notebook!))

    (defun change-cell-type (cell-id new-type)
      (console.log "CHANGING TYPE OF" cell-id "TO" new-type)
      (post/json "/notebook/change-cell-type" (create :book (notebook-name *notebook*) :cell-id cell-id :new-type new-type)
		 #'notebook!))

    (defun reorder-cells (ev)
      (prevent ev)
      (let ((ord (obj->string
		  (loop for elem in (by-selector-all ".cell")
		     collect (parse-int (chain elem (get-attribute :cell-id)))))))
	(post "/notebook/reorder-cells" 
	      (create :book (notebook-name *notebook*) 
		      :cell-order ord))))

    ;; CodeMirror utilities    
    (defun show-editor (cell-id)
      (setf (@ (by-selector (+ "#cell-" cell-id " .CodeMirror")) hidden) nil)
      (chain (aref *notebook* :objects cell-id :editor) (focus)))

    (defun hide-editor (cell-id)
      (setf (@ (by-selector (+ "#cell-" cell-id " .CodeMirror")) hidden) t))

    (defun toggle-editor (cell-id)
      (setf (@ (by-selector (+ "#cell-" cell-id " .CodeMirror")) hidden)
	    (not (@ (by-selector (+ "#cell-" cell-id " .CodeMirror")) hidden))))

    (defun cell-mirror (cell-id)
      (aref *notebook* :objects cell-id :editor))
    
    (defun cell-editor-contents (cell-id)
      (chain (cell-mirror cell-id) (get-value)))

    (defun mirror-keys (cell-id &key hiding?)
      (let ((keys (create "Ctrl-Enter"
			  (lambda (cmd)
			    (server/notebook/eval-to-cell
			     cell-id (cell-editor-contents cell-id)))
			  "Ctrl-Space" "autocomplete")))
	(when hiding?
	  (setf (@ keys "Esc")
		(lambda (cmd) (hide-editor cell-id))))
	keys))

    (defun mirror! (cell-id keys)
      (let* ((mirror)
	     (options (create 
		       "lineNumbers" t 
		       "matchBrackets" t
		       "autoCloseBrackets" t
		       "viewportMargin" -infinity
		       "extraKeys" keys)))
	(setf 
	 mirror 
	 (chain -code-mirror 
		(from-text-area 
		 (by-selector (+ "#cell-" cell-id " .cell-contents"))
		 options)))
	mirror))

    ;; Notebook-related
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
      (let* ((ord (new (-array)))
	     (obj (notebook-objects notebook))
	     (in-obj? (lambda (id) (in id obj)))
	     (implicit (new (-array))))
	(loop for (a b c) in (notebook-facts notebook)
	   when (equal b "cellOrder") do (setf ord c)
	   when (and (equal b :cell) (null c))
	   do (chain implicit (push a)))
	(append-new 
	 (filter in-obj? ord) 
	 (chain implicit (reverse)))))
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
	(nativesortable (by-selector "ul.cells"))
	(map (lambda (cell) 
	       (with-slots (id cell-type) cell
		 (setf (aref *notebook* :objects id :editor)
		       (mirror! 
			id (if (equal cell-type "clWho") 
			       (mirror-keys id :hiding? t)
			       (mirror-keys id))))
		 (when (equal cell-type "clWho")
		   (setf (@ (by-selector (+ "#cell-" id " .CodeMirror")) hidden) t))))
	     (notebook-cells *notebook*))))

    (dom-ready
     (lambda ()
       (set-page-hash (create :book "test-book"))
       (setf document.title "test-book - cl-notebook")
       (server/notebook/current "test-book" #'notebook!)))))
