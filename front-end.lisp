(in-package :cl-notebook)

(define-handler (root) ()
  (with-html-output-to-string (s nil :prologue t :indent t)
    (:html
     (:head
      (:title "cl-notebook")
      
      (:link :rel "stylesheet" :href "/css/notebook.css")
      (:link :rel "stylesheet" :href "/static/css/genericons.css")
      (:link :rel "stylesheet" :href "/static/css/codemirror.css")
      (:link :rel "stylesheet" :href "/static/css/dialog.css")
      (:link :rel "stylesheet" :href "/static/css/show-hint.css")

      (:script :type "text/javascript" :src "/static/js/Blob.js")
      (:script :type "text/javascript" :src "/static/js/FileSaver.js")
      
      (:script :type "text/javascript" :src "/js/base.js")
      (:script :type "text/javascript" :src "/js/templates.js")
      (:script :type "text/javascript" :src "/js/ajax.js")
      (:script :type "text/javascript" :src "/js/main.js")
      (:script :type "text/javascript" :src "/js/book-actions.js")
      (:script :type "text/javascript" :src "/static/js/native-sortable.js")

      (:script :type "text/javascript" :src "/static/js/codemirror.js")
      
      (:script :type "text/javascript" :src "/static/js/modes/commonlisp.js")
      (:script :type "text/javascript" :src "/static/js/addons/closebrackets.js")
      (:script :type "text/javascript" :src "/static/js/addons/matchbrackets.js")
      (:script :type "text/javascript" :src "/static/js/addons/search.js")
      (:script :type "text/javascript" :src "/static/js/addons/searchcursor.js")
      (:script :type "text/javascript" :src "/static/js/addons/match-highlighter.js")
      (:script :type "text/javascript" :src "/static/js/addons/active-line.js")
      (:script :type "text/javascript" :src "/static/js/addons/mark-selection.js")
      (:script :type "text/javascript" :src "/static/js/addons/show-hint.js")
      (:script :type "text/javascript" :src "/static/js/addons/anyword-hint.js")
      (:script :type "text/javascript" :src "/static/js/addons/dialog.js")
      (:script :type "text/javascript" :src "/static/js/addons/runmode/runmode.js"))

     (:body
      (:div :class "main-controls"
	    (:input :id "book-history-slider" :type "range" :min 0 :max 500 :value 500)
	    (:input :id "book-history-text" :onchange "rewindBook(this.value)")
	    (:button :onclick "newCell()" "+ New Cell")
	    (:button :onclick "newBook()" "+ New Book")
	    (:select :id "book-list"
		     :onchange "displayBook(this.value)"
		     (:option :value "" "Choose book...")
		     (loop for name being the hash-keys of *notebooks*
			do (htm (:option :value name (str name)))))
	    (:select :id "book-actions"
		     :onchange "runBookAction(this.value)"
		     (:option :value "" "Stuff...")
		     (:optgroup 
		      :label "Export"
		      (:option :value "export-html" "Export as HTML")
		      (:option :value "export-lisp" "Export as .lisp"))
		     (:optgroup
		      :label "Delete"
		      (:option :value "kill-book" "Kill Book"))))
      
      (:div :id "notebook")
      (:div :class "footer"
	    (:span :class "notice" "Processing")
	    (:img :src "/static/img/dots.png")
	    (:button :onclick "killThread()" :class "right" "! Abort"))))))

(define-handler (js/base.js :content-type "application/javascript") ()
  (ps 
    ;;;; base.js contains general utilities that might be useful in other JS
    ;;;; applications too. Nothing notebook-specific here.

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
      (let ((s (new (-set list-a)))
	    (lst (map #'identity list-a)))
	(loop for elem in list-b
	   unless (chain s (has elem))
	   do (chain lst (push elem)))
	lst))

    (defun join (strings &optional (separator "")) (chain strings (join separator)))

    ;; basic hash/array stuff
    (defun vals (obj) (map identity obj))
    (defun keys (obj) (map (lambda (v k) k) obj))
    (defun last (array) (aref array (- (length array) 1)))

    ;; basic DOM/event stuff
    (defun dom-ready (callback)
      (chain document (add-event-listener "DOMContentLoaded" callback)))

    (defun remove-all-event-handlers (elem)
      (let ((clone (chain elem (clone-node t))))
	(chain elem parent-node (replace-child clone elem))
	clone))

    (defun prevent (ev) (chain ev (prevent-default)))

    (defun scroll-to-elem (elem)
      (let ((x (@ elem offset-left))
	    (y (@ elem offset-top)))
	(chain window (scroll-to x y))))

    (defun show! (elem)
      (setf (@ elem hidden) nil))
    (defun hide! (elem)
      (setf (@ elem hidden) t))

    (defun by-selector (selector)
      (chain document (query-selector selector)))
    (defun by-selector-all (selector)
      (chain document (query-selector-all selector)))

    (defun dom-escape (string)
      (chain string
	     (replace "<" "&lt;")
	     (replace ">" "&gt;")))

    (defun dom-append (elem markup)
      (let ((new-content (chain document (create-element "span"))))
	(setf (@ new-content inner-h-t-m-l) markup)
	(loop while (@ new-content first-child)
	   do (chain elem (append-child (@ new-content first-child))))))

    (defun dom-replace (elem markup)
      (let ((new-content (chain document (create-element "span"))))
	(setf (@ new-content inner-h-t-m-l) markup)
	(chain elem parent-node (replace-child new-content elem))))

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
    (defun save-file (filename contents &optional (type "application/json;charset=utf-8"))
      (let* ((content-string (if (string? contents) contents (obj->string contents)))
	     (blob (new (-blob (list content-string) (create :type type)))))
	(save-as blob filename)))

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

    (defun post (uri params on-success on-fail)
      (let ((req (new (-x-m-l-http-request)))
	    (encoded-params (obj->params params)))
	(setf (@ req onreadystatechange)
	      (lambda ()
		(when (equal (@ req ready-state) 4)
		  (if (equal (@ req status) 200)
		      (when (function? on-success)
			(let ((result (@ req response-text)))
			  (on-success result)))
		      (when (function? on-fail)
			(on-fail req))))))
	(chain req (open :POST uri t))
	(chain req (set-request-header "Content-type" "application/x-www-form-urlencoded"))
	(chain req (set-request-header "Content-length" (length encoded-params)))
	(chain req (set-request-header "Connection" "close"))
	(chain req (send encoded-params))))

    (defun post/json (uri params on-success on-fail)
      (post uri params
	    (lambda (raw)
	      (when (function? on-success)
		(let ((res (string->obj raw)))
		  (on-success res))))
	    on-fail))

    (defun event-source (uri bindings)
      (let ((stream (new (-event-source uri))))
	(setf (@ stream onopen) (lambda (e) (console.log "Stream OPENED!"))
	      (@ stream onerror) (lambda (e) (console.log "Stream ERRORED!"))
	      (@ stream onmessage)
	      (lambda (e)
		(let* ((res (string->obj (@ e data)))
		       (callback (aref bindings (@ res action))))
		  (if callback
		      (funcall callback res)
		      (console.log "Unhandled message" res)))))
	stream))))

(define-handler (js/templates.js :content-type "application/javascript") ()
  (ps 
    (defun condition-template (err)
      (who-ps-html
       (:ul :class "condition-contents"
	    (:li :class "condition-type" (@ err condition-type))
	    (:li :class "condition-form" (@ err form))
	    (join (map 
		   (lambda (v k) 
		     (if (and v (not (or (= k "conditionType") (= k "form"))))
			 (who-ps-html 
			  (:li :class "condition-property" 
			       (:span :class "label" k ":") (dom-escape v)))
			 ""))
		      err)))))

    (defun result-values-template (result-vals)
      (when result-vals
	(who-ps-html
	 (:ul :onclick "selectContents(event, this)" :class "result"
	      (join (loop for v in result-vals
		       collect (with-slots (type value) v
				 (if (= type :error)
				     (who-ps-html (:li :class "error" (condition-template value)))
				     (who-ps-html (:li (:span :class "value" (dom-escape value))
						       (:span :class "type" " :: " type)))))))))))
    
    (defun result-warnings-template (result-warnings)
      (who-ps-html
       (:span :onclick "selectContents(event, this)" :class "warnings"
	      (join (loop for w in result-warnings 
		       collect (condition-template w))))))

    (defun result-stdout-template (stdout)
      (if stdout
	  (who-ps-html
	   (:p :onclick "selectContents(event, this)" :class "stdout"
	       stdout))
	  ""))

    (defun terse-result-template (results)
      (result-values-template (@ (last results) values)))

    (defun normal-result-template (results)
      (let ((all-stdout (new (-array)))
	    (all-warnings (new (-array)))
	    (res))
	(loop for r in results
	   do (with-slots (stdout warnings) r
		(chain all-stdout (push stdout))
		(when warnings
		  (loop for w in warnings
		     unless (*warning-filter* w) 
		     do (chain all-warnings (push w)))))
	   finally (setf res (@ r values))))
      (who-ps-html
       (result-stdout-template (join all-stdout))
       (result-warnings-template all-warnings)
       (result-values-template res)))

    (defun verbose-result-template (results)
      (join
       (loop for res in results
	  when (@ res stdout)
	  collect (result-stdout-template (@ res stdout))
	  when (@ res warnings)
	  collect (result-warnings-template (@ res warnings))
	  when (@ res values)
	  collect (result-values-template (@ res values)))))

    (defun result-template (noise result)
      (when result
	(who-ps-html 
	 (:pre (case noise
		 (:verbose 
		  (verbose-result-template result))
		 (:terse
		  (terse-result-template result))
		 (:silent "")
		 (t (normal-result-template result)))))))

    (defun cell-controls-template (cell)
      (who-ps-html
       (:div :class "controls" 
	     (:span :class "genericon genericon-draggable")
	     (:button :class "genericon genericon-trash" 
		      :onclick (+ "killCell(" (@ cell id) ")") "  ")
	     (:select 
	      :onchange (+ "changeCellType(" (@ cell id) ", this.value)")
	      (join (loop for tp in (list :markup :code :tests)
		       if (= (@ cell cell-type) tp)
		       collect (who-ps-html (:option :value tp :selected "selected" tp))
		       else collect (who-ps-html (:option :value tp tp)))))
	     ;; (:select 
	     ;;  :onchange (+ "changeCellLanguage(" (@ cell id) ", this.value)")
	     ;;  (join (loop for lang in (list :common-lisp)
	     ;; 	       for lb in (list 'common-lisp) ;; curse these symbol case issues
	     ;; 	       if (= (@ cell cell-type) lb)
	     ;; 	       collect (who-ps-html (:option :value lang :selected "selected" lang))
	     ;; 	       else 
	     ;; 	       collect (who-ps-html (:option :value lang lang)))))
	     (:select
	      :onchange (+ "changeCellNoise(" (@ cell id) ", this.value)")
	      (join (loop for ns in (list :silent :terse :normal :verbose)
		       if (or (and (@ cell noise) (= (@ cell noise) ns))
			      (and (not (@ cell noise)) (= ns :normal)))
		       collect (who-ps-html (:option :value ns :selected "selected" ns))
		       else
		       collect (who-ps-html (:option :value ns ns))))))))
    
    (defun cell-markup-result-template (result)
      (if result
	  (let ((val (@ result 0 values 0 value)))
	    (cond ((and (string? val) (= "" val))
		   (who-ps-html (:p (:b "[[EMPTY CELL]]"))))
		  ((string? val) val)
		  (t (result-template :verbose result))))
	  (who-ps-html (:p (:b "[[EMPTY CELL]]")))))

    (defun cell-markup-template (cell)
      (with-slots (id contents result language) cell
	(who-ps-html 
	 (:li :class (+ "cell markup" (if (@ cell stale) " stale" "")) :id (+ "cell-" id) :cell-id id 
	      :ondragend "reorderCells(event)" :draggable "true"
	      (cell-controls-template cell)
	      (:textarea :class "cell-contents" :language (or language "commonlisp") contents)
	      (:div :onclick (+ "showEditor(" id ")")
		    (:span :class "cell-value" (cell-markup-result-template result)))))))
    
    (defun cell-code-template (cell)
      (with-slots (id contents result language) cell
	(who-ps-html 
	 (:li :class (+ "cell code" (if (@ cell stale) " stale" "")) :id (+ "cell-" id) :cell-id id 
	      :ondragend "reorderCells(event)" :draggable "true"
	      (cell-controls-template cell)
	      (:textarea :class "cell-contents" :language (or language "commonlisp")  contents)
	      (:span :class "cell-value"
		     (result-template (@ cell noise) result))))))
    
    (defun cell-template (cell)
      (if (markup-cell? cell)
	  (cell-markup-template cell)
	  (cell-code-template cell)))
    
    (defun show-title-input () 
      (let ((input (by-selector ".book-title input"))))
      (show! input)
      (chain input (focus))
      (chain input (select))
      (hide! (by-selector ".book-title h1")))

    (defun hide-title-input () 
      (hide! (by-selector ".book-title input"))
      (show! (by-selector ".book-title h1")))
    
    (defun notebook-title-template (name)
      (who-ps-html
       (:div :class "book-title"
	     (:input :class "text" :onchange "renameBook(this.value)" :value name)
	     (:h1 :onclick "showTitleInput()" name))))

    (defun notebook-template (notebook)
      (+ (notebook-title-template (notebook-name notebook))
	 (who-ps-html
	  (:ul :class "cells"
	       (join (map (lambda (cell) (cell-template cell))
			  (notebook-cells notebook)))))))))

(define-handler (js/ajax.js :content-type "application/javascript") ()
  (ps (defun kill-thread ()
	(post/json "/cl-notebook/system/kill-thread" (create)))
      
      (defun rewind-book (index)
	(post/json "/cl-notebook/notebook/rewind" (create :book (notebook-name *notebook*) :index index)
		   #'notebook!))

      (defun notebook/current (name)
	(post/json "/cl-notebook/notebook/current" (create :book name)
		   #'notebook!
		   (lambda (res)
		     ($aif (by-selector "#book-list option[selected='selected']")
			   (chain it (remove-attribute :selected)))
		     (setf (@ (by-selector "#book-list") selected-index) 0)
		     (dom-set 
		      (by-selector "#notebook")
		      (who-ps-html (:h2 "Notebook '" name "' not found..."))))))

      (defun new-book () 
	(post/json "/cl-notebook/notebook/new" (create) 
		   #'notebook!))

      (defun kill-book ()
	(post/json "/cl-notebook/notebook/kill" (create :book (notebook-name *notebook*))))

      (defun rename-book (new-name)
	(post/json "/cl-notebook/notebook/rename" (create :book (notebook-name *notebook*) :new-name new-name)))

      (defun notebook/eval-to-cell (cell-id contents)
	(post/json "/cl-notebook/notebook/eval-to-cell" (create :book (notebook-name *notebook*) :cell-id cell-id :contents contents)))

      (defun new-cell (&optional (cell-language :common-lisp) (cell-type :code))
	(post/json "/cl-notebook/notebook/new-cell" (create :book (notebook-name *notebook*) :cell-type cell-type :cell-language cell-language)))

      (defun kill-cell (cell-id)
	(post/json "/cl-notebook/notebook/kill-cell" (create :book (notebook-name *notebook*) :cell-id cell-id)))
      
      (defun change-cell-contents (cell-id contents)
	(post/json "/cl-notebook/notebook/change-cell-contents" (create :book (notebook-name *notebook*) :cell-id cell-id :contents contents)))

      (defun change-cell-type (cell-id new-type)
	(post/json "/cl-notebook/notebook/change-cell-type" (create :book (notebook-name *notebook*) :cell-id cell-id :new-type new-type)))
      
      (defun change-cell-language (cell-id new-language)
	(post/json "/cl-notebook/notebook/change-cell-language" (create :book (notebook-name *notebook*) :cell-id cell-id :new-language new-language)))

      (defun change-cell-noise (cell-id new-noise)
	(post/json "/cl-notebook/notebook/change-cell-noise" (create :book (notebook-name *notebook*) :cell-id cell-id :new-noise new-noise)))

      (defun reorder-cells (ev)
	(prevent ev)
	(let ((ord (obj->string
		    (loop for elem in (by-selector-all ".cell")
		       collect (parse-int (chain elem (get-attribute :cell-id)))))))
	  (post "/cl-notebook/notebook/reorder-cells" 
		(create :book (notebook-name *notebook*) 
			:cell-order ord))))))

(define-handler (js/book-actions.js :content-type "application/javascript") ()
  (ps 
    (defvar *book-actions*
      (create :export-html
	      (lambda ()
		(save-file
		 (+ (notebook-name *notebook*) ".html")
		 (+
		  (who-ps-html 
		   (:style :type "text/css" :media "screen"
			   "<!--
.cm-s-default .cm-keyword {color: #708;}
.cm-s-default .cm-atom {color: #219;}
.cm-s-default .cm-number {color: #164;}
.cm-s-default .cm-def {color: #00f;}
.cm-s-default .cm-variable {color: black;}
.cm-s-default .cm-variable-2 {color: #05a;}
.cm-s-default .cm-variable-3 {color: #085;}
.cm-s-default .cm-property {color: black;}
.cm-s-default .cm-operator {color: black;}
.cm-s-default .cm-comment {color: #a50;}
.cm-s-default .cm-string {color: #a11;}
.cm-s-default .cm-string-2 {color: #f50;}
.cm-s-default .cm-meta {color: #555;}
.cm-s-default .cm-qualifier {color: #555;}
.cm-s-default .cm-builtin {color: #30a;}
.cm-s-default .cm-bracket {color: #997;}
.cm-s-default .cm-tag {color: #170;}
.cm-s-default .cm-attribute {color: #00c;}
.cm-s-default .cm-header {color: blue;}
.cm-s-default .cm-quote {color: #090;}
.cm-s-default .cm-hr {color: #999;}
.cm-s-default .cm-link {color: #00c;}
.cm-s-default .cm-error {color: #f00;}

.result { border: 1px solid #ccc; background-color: #fff; list-style-type: none; margin: 0px; margin-top: 5px; padding: 0px; }
.stdout { margin: 0px; padding: 5px; color: #8b2252; background-color: #efefef; }
.result li { padding: 5px; }
.result .type { color: #228b22; }
.warnings .condition-contents { background-color: #fc6; color: #c60; border: 1px solid #c60; padding: 5px; margin: 5px 0px; }
.result .error { background-color: #fdd; color: #933; }
.condition-contents { list-style-type: none; margin: 0px; padding: 0px; }
.condition-contents .condition-type { font-weight: bolder; }
.condition-contents .condition-property { font-style: oblique; }
.condition-contents .condition-property .label { display: inline-block; margin-right: 5px; font-size: small; }
-->")
		   (:h1 (notebook-name *notebook*)))
		  (join 
		   (map (lambda (cell)
			  (if (= 'markup (@ cell cell-type))
			      (@ cell result 0 values 0 value)
			      (let ((node (chain document (create-element "pre"))))
				(chain node (set-attribute :class "cm-s-default"))
				(chain -code-mirror (run-mode (@ cell contents) "commonlisp" node))
				(+ (@ node outer-h-t-m-l)
				   (if (not (= 'silent (@ cell noise)))
				       ($aif (by-cell-id (@ cell id) ".cell-value" "pre")
					     (@ it outer-h-t-m-l)
					     "")
				       "")))))
			(notebook-cells *notebook*))))
		 "text/html;charset=utf-8"))
	      :export-lisp
	      (lambda ()
		(save-file 
		 (+ (notebook-name *notebook*) ".lisp")
		 (+ "; Generated by cl-notebook from " (notebook-name *notebook*) ".base"
		    (join 
		     (loop for cell in (notebook-cells *notebook*)
			when (and (= 'common-lisp (@ cell cell-language))
				  (= 'code (@ cell cell-type)))
			collect (+ #\newline #\newline
				   ";;; Cell " (@ cell id)
				   #\newline
				   (@ cell contents)))))
		 "text/x-common-lisp;charset=utf-8"))
	      :kill-book 
	      #'kill-book))
    
    (defun run-book-action (action)
      (setf (@ (by-selector "#book-actions") selected-index) 0)
      ($aif (aref *book-actions* action)
	    (funcall it)
	    (console.log "NOT YET IMPLEMENTED: " action)))))

(define-handler (js/main.js :content-type "application/javascript") ()
  (ps
    ;; cl-notebook specific utility
    (defun by-cell-id (cell-id &rest children)
      (by-selector
       (+ "#cell-" cell-id
	  (if (> (length children) 0) " " "")
	  (join children " "))))

    (defun markup-cell? (cell)
      (= 'markup (@ cell cell-type)))
    
    (defun clear-selection ()
      (let ((sel (chain window (get-selection))))
	(if (@ sel empty)
	    ;; chrome
	    (chain sel (empty))
	    ;; firefox
	    (chain sel (remove-all-ranges)))))

    (defun select-contents (ev elem)
      (unless (@ ev shift-key)
	(clear-selection))
      (let ((r (new (-range))))
	(chain r (select-node-contents elem))
	(chain window (get-selection) (add-range r))))

    (defun debounced-save (cell-id delay)
      (let ((last-run (new -date))
	    (save! 
	     (lambda () 
	       (change-cell-contents cell-id (cell-editor-contents cell-id))))
	    (parting-shot))
	(lambda (mirror change)
	  (let ((now (new -date)))
	    (when (or (= "+input" (@ change origin)) (= "+delete" (@ change origin)))
	      (chain (by-cell-id cell-id) class-list (add "stale"))
	      (clear-timeout parting-shot)
	      (setf parting-shot (set-timeout #'save! delay))
	      (when (> (- now last-run) (* delay 2))
		(setf last-run now)
		(funcall save!)))))))

    ;; cl-notebook specific DOM manipulation
    (defun display-book (book-name)
      (when book-name
	(set-page-hash (create :book book-name))
	(hash-updated)))

    (defun hash-updated ()
      (let ((book-name (@ (get-page-hash) :book)))
	(when book-name
	  (setf document.title (+ book-name " - cl-notebook"))
	  (notebook/current book-name))))

    (defun dom-replace-cell-value (cell)
      (when (@ cell result)
	(let ((res (@ cell result result)))
	  (dom-set (by-cell-id (@ cell :id) ".cell-value")
		   (if (markup-cell? cell)
		       (cell-markup-result-template (@ cell result))
		       (result-template (@ cell noise) (@ cell result) :stale? (@ cell stale)))))))
    
    (defun dom-replace-cell (cell)
      (dom-replace (by-cell-id (@ cell id)) (cell-template cell))
      (mirror! cell))

    ;; CodeMirror and utilities
    (defun show-editor (cell-id)
      (show! (by-cell-id cell-id ".CodeMirror"))
      (chain (cell-mirror cell-id) (focus)))

    (defun hide-editor (cell-id)
      (hide! (by-cell-id cell-id ".CodeMirror")))

    (defun cell-mirror (cell-id)
      (@ (notebook-cell *notebook* cell-id) editor))
    
    (defun cell-editor-contents (cell-id)
      (chain (cell-mirror cell-id) (get-value)))

    (defun mirror! (cell)
      (let* ((mirror)
	     (cell-id (@ cell id))
	     (options (create
		       "lineNumbers" t
		       "matchBrackets" t
		       "autoCloseBrackets" t
		       "lineWrapping" t
		       "viewportMargin" -infinity
		       "extraKeys" (create "Ctrl-Enter"
					   (lambda (cmd)
					     (notebook/eval-to-cell
					      cell-id (cell-editor-contents cell-id)))
					   "Ctrl-Space" "autocomplete"))))
	(setf 
	 mirror (chain -code-mirror (from-text-area (by-cell-id cell-id ".cell-contents") options))
	 (@ cell editor) mirror)
	(when (= :markup (@ cell cell-type))
	  (chain mirror (on :change (debounced-save cell-id 4000))))
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

    (defun notebook-name (notebook) (@ notebook name))
    (defun set-notebook-name (notebook new-name) (setf (@ notebook name) new-name))

    (defun notebook-facts (notebook) (@ notebook facts))
    (defun notebook-objects (notebook) (@ notebook objects))

    (defun notebook-cell-ordering (notebook)
      (let ((ord (loop for (a b c) in (notebook-facts notebook)
		    when (= b 'cell-order) do (return c)))
	    (all-cell-ids 
	     (loop for (a b c) in (notebook-facts notebook) 
		when (and (= b 'cell) (null c)) collect a)))
	(chain all-cell-ids (reverse))
	(if ord
	    (append-new ord all-cell-ids)
	    all-cell-ids)))

    (defun notebook-cells (notebook)
      (let ((obj (notebook-objects notebook))
	    (ord (notebook-cell-ordering notebook)))
	(loop for id in ord for res = (aref obj id)
	   when res collect res)))

    (defun notebook-cell (notebook id)
      (aref notebook :objects id))
    
    (defun notebook! (raw)
      (let* ((fs (@ raw facts))
	     (count (@ raw history-size))
	     (pos (or (@ raw history-position) count)))
	(setf *notebook* 
	      (create :facts fs :objects (notebook-condense fs)
		      :history-size count
		      :name (loop for (a b c) in fs
			       when (equal b "notebookName")
			       do (return c))))
	(dom-set 
	 (by-selector "#notebook")
	 (notebook-template *notebook*))
	(let ((slider (remove-all-event-handlers (by-selector "#book-history-slider"))))
	  (console.log "Resetting history values...")
	  (chain slider (set-attribute :max count))
	  (setf (@ slider value) pos
		(@ (by-selector "#book-history-text") value) pos)
	  (chain slider (add-event-listener :change (lambda () (rewind-book (@ slider value))))))
	(hide! (by-selector ".book-title input"))
	(nativesortable (by-selector "ul.cells"))
	(set-page-hash (create :book (notebook-name *notebook*)))
	(map (lambda (opt) (chain opt (remove-attribute :selected)))
	     (by-selector-all "#book-list option"))
	(chain (by-selector (+ "#book-list option[value='" (notebook-name *notebook*) "']"))
	       (set-attribute :selected "selected"))
	(map (lambda (cell) 
	       (with-slots (id cell-type) cell
		 (mirror! cell)
		 (when (= 'markup cell-type)
		   (hide! (by-cell-id id ".CodeMirror")))))
	     (notebook-cells *notebook*))))

    (defun notebook-events ()
      (event-source 
       "/cl-notebook/source"
       (create
	'new-cell 
	(lambda (res)
	  (when (equal (notebook-name *notebook*) (@ res book))
	    (let ((id (@ res 'cell-id))
		  (cell (create 'type "cell" 'contents "" 'result ""
				'cell-type (@ res cell-type) 
				'cell-language (@ res cell-language)
				'id (@ res 'cell-id))))
	      (setf (aref (notebook-objects *notebook*) id) cell)
	      (chain (notebook-facts *notebook*) (unshift (list id 'cell nil)))
	      (dom-append (by-selector ".cells")
			  (cell-template cell))
	      (mirror! cell)
	      (scroll-to-elem (by-cell-id id))
	      (show-editor id))))
	'change-cell-type
	(lambda (res)
	  (when (equal (notebook-name *notebook*) (@ res book))
	    (let ((cell (notebook-cell *notebook* (@ res cell))))
	      (setf (@ cell cell-type) (@ res new-type))
	      (dom-replace-cell cell))))
	'change-cell-language
	(lambda (res)
	  (when (equal (notebook-name *notebook*) (@ res book))
	    (let ((cell (notebook-cell *notebook* (@ res cell))))
	      (setf (@ cell cell-language) (@ res new-language))
	      (dom-replace-cell cell))))
	'change-cell-noise
	(lambda (res)
	  (when (equal (notebook-name *notebook*) (@ res book))
	    (let ((cell (notebook-cell *notebook* (@ res cell))))
	      (setf (@ cell noise) (@ res new-noise))
	      (dom-replace-cell-value cell))))
	'starting-eval
	(lambda (res)
	  (show! (by-selector ".footer")))
	'killed-eval
	(lambda (res)
	  (hide! (by-selector ".footer")))
	'finished-eval 
	(lambda (res)
	  (hide! (by-selector ".footer"))
	  (when (equal (notebook-name *notebook*) (@ res book))
	    (let ((cell (notebook-cell *notebook* (@ res cell))))
	      (setf (@ cell contents) (@ res contents)
		    (@ cell result) (@ res result))
	      (delete (@ cell stale))
	      (chain (by-cell-id (@ res cell)) class-list (remove "stale"))
	      (dom-replace-cell-value cell))))
	'content-changed
	(lambda (res)
	  (when (equal (notebook-name *notebook*) (@ res book))
	    (let* ((cell (notebook-cell *notebook* (@ res cell)))
		   (mirror (cell-mirror (@ res cell)))
		   (cursor (chain mirror (get-cursor))))
	      (setf (@ cell contents) (@ res contents)
		    (@ cell stale) t)
	      (chain (by-cell-id (@ res cell)) class-list (add "stale"))
	      (chain mirror (set-value (@ res contents)))
	      (chain mirror (set-cursor cursor)))))
	'kill-cell 
	(lambda (res)
	  (when (equal (notebook-name *notebook*) (@ res book))
	    (delete (aref *notebook* 'objects (@ res cell)))
	    (chain (by-cell-id (@ res cell)) (remove))))
	
	'reorder-cells 
	(lambda (res)
	  (when (equal (notebook-name *notebook*) (@ res book))
	    ;; TODO change order here to support proper multi-user noting
	    (console.log "Changed cell order" res)))
	
	'new-book
	(lambda (res)
	  (let ((name (@ res book-name)))
	    (dom-append (by-selector "#book-list")
			(who-ps-html (:option :value name name)))))
	'kill-book
	(lambda (res)
	  (let ((name (@ res book)))
	    (chain (by-selector (+ "#book-list option[value='" name "']")) (remove))
	    (when (equal (notebook-name *notebook*) name)
	      (display-book 
	       (chain (@ (by-selector-all "#book-list option") 1)
		      (get-attribute :value))))))
	'rename-book
	(lambda (res)
	  (let ((old-name (@ res book))
		(new-name (@ res new-name)))
	    (when (equal (notebook-name *notebook*) old-name)
	      (dom-replace (by-selector ".book-title")
			   (notebook-title-template new-name))
	      (set-notebook-name *notebook* new-name)
	      (set-page-hash (create :book new-name))
	      (hide-title-input))
	    (chain (by-selector (+ "#book-list option[value='" old-name "']")) (remove))
	    (dom-append (by-selector "#book-list")
			(who-ps-html (:option :value new-name new-name))))))))

    (defvar *warning-filter* 
      (lambda (w)
	(or (chain (@ w condition-type) (starts-with "REDEFINITION"))
	    (and (@ w error-message) 
		 (or  (chain (@ w error-message) (starts-with "undefined "))
		      (chain (@ w error-message) (ends-with "never used.")))))))

    (dom-ready
     (lambda ()
       (notebook-events)
       (hide! (by-selector ".footer"))
       (chain 
	(by-selector "body") 
	(add-event-listener 
	 :keyup (key-listener
		 <esc> (progn 
			 (clear-selection)
			 (hide-title-input)
			 (map (lambda (cell)
				(with-slots (id cell-type) cell
				  (when (= 'markup cell-type)
				    (hide-editor id))))
			      (notebook-cells *notebook*))))))

       (unless (get-page-hash)
	 (set-page-hash (create :book (chain (@ (by-selector-all "#book-list option") 1) (get-attribute :value)))))
       (setf (@ window onhashchange) #'hash-updated)
       (hash-updated)))))
