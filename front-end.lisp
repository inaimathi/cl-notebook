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
      (:script :type "text/javascript" :src "/js/pareditesque.js")
      (:script :type "text/javascript" :src "/js/book-actions.js")
      (:script :type "text/javascript" :src "/static/js/native-sortable.js")

      (:script :type "text/javascript" :src "/static/js/codemirror.js")
      
      (:script :type "text/javascript" :src "/static/js/modes/commonlisp.js")
      (:script :type "text/javascript" :src "/static/js/addons/comment.js")
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
	    (:input :id "book-history-slider" :onchange "rewindBook(this.value)" :oninput "debouncedRewind(this.value)" :type "range" :min 0 :max 500 :value 500)
	    (:input :id "book-history-text" :onchange "rewindBook(this.value)")
	    (:button :onclick "newCell()" "+ New Cell")
	    (:button :onclick "newBook()" "+ New Book")
	    (:select :id "book-list"
		     :onchange "displayBook(this.value)"
		     (:option :value "" "Choose book...")
		     ;; (loop for (id name) in (ordered-books)
		     ;; 	do (htm (:option :value id (str name))))
		     (loop for id being the hash-keys of *notebooks*
			for book being the hash-values of *notebooks*
			do (htm (:option :value id (str (notebook-name book))))))
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
      (:div :id "macro-expansion" (:textarea :language "commonlisp"))
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

    (defun lines (string) (chain string (split #\newline)))

    (defun join (strings &optional (separator "")) (chain strings (join separator)))

    ;; basic hash/array stuff
    (defun vals (obj) (map identity obj))
    (defun keys (obj) (map (lambda (v k) k) obj))
    (defun last (array) (aref array (- (length array) 1)))

    (defun member? (elem thing)
      (if (object? thing)
	  (in elem thing)
	  (chain thing (index-of elem))))

    (defun equal? (a b)
      (let ((type-a (typeof a))
	    (type-b (typeof b)))
	(and (equal type-a type-b)
	     (cond
	       ((member? type-a (list "number" "string" "function"))
		(equal a b))
	       ((array? a)
		(and
		 (= (length a) (length b))
		 (loop for elem-a in a for elem-b in b
		    unless (equal? elem-a elem-b) return f
		    finally (return t))))
	       ((object? a)
		;; object comparison here
		;; and 
		;;   all keys of a are in b
		;;   all keys of b are in a
		;;   all keys of a and b have the same values
		nil
		)))))

    ;; basic regex stuff
    (defun regex-match (regex string)
      (chain (-reg-exp regex) (test string)))
    (defun regex-match-any (string &rest regexes)
      (loop for reg in regexes
	 when (regex-match reg string) return t
	 finally (return nil)))

    (defun matching? (regex)
      (lambda (string) (regex-match regex string)))

    ;; basic DOM/event stuff
    (defun sheet-text (sheet &optional (predicate identity))
      (if (number? sheet)
	  (sheet-text (aref (@ document style-sheets) sheet) predicate)
	  (join
	   (loop for rule in (@ sheet css-rules)
	      for text = (@ rule css-text)
	      when (predicate text) collect text)	   
	   #\newline)))
    
    (defun dom-ready (callback)
      (chain document (add-event-listener "DOMContentLoaded" callback)))

    (defun remove-all-event-handlers (elem)
      (let ((clone (chain elem (clone-node t))))
	(chain elem parent-node (replace-child clone elem))
	clone))

    (defun prevent (ev) (when ev (chain ev (prevent-default))))

    (defun debounce (fn delay immediate?)
      (let ((timeout))
	(lambda ()
	  (let ((context this)
		(args arguments))
	    (clear-timeout timeout)
	    (setf timeout (set-timeout 
			   (lambda ()
			     (setf timeout nil)
			     (unless immediate?
			       (chain fn (apply context args))))
			   delay))
	    (when (and immediate? (not timeout))
	      (chain fn (apply context args)))))))

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
      (when (string? string)
	(chain string
	       (replace "<" "&lt;")
	       (replace ">" "&gt;"))))

    (defun dom-append (elem markup)
      (let ((new-content (chain document (create-element "span"))))
	(setf (@ new-content inner-h-t-m-l) markup)
	(loop while (@ new-content first-child)
	   do (chain elem (append-child (@ new-content first-child))))))

    (defun dom-replace (elem markup)
      (let ((new-content (chain document (create-element "span")))
	    (parent (@ elem parent-node)))
	(setf (@ new-content inner-h-t-m-l) markup)
	(loop for child in (@ new-content child-nodes)
	   do (chain parent (insert-before new-content elem)))
	(chain elem (remove))))

    (defun dom-set (elem markup)
      (setf (@ elem inner-h-t-m-l) markup))

    ;; basic type stuff
    (defun number? (obj) (string= "number" (typeof obj)))
    (defun string? (obj) (string= "string" (typeof obj)))
    (defun function? (obj) (string= "function" (typeof obj)))

    (defun type? (obj type-string)
      (eql (chain -object prototype to-string (call obj)) type-string))
    (defun array? (arr) (type? arr "[object Array]"))
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

    (defun show-footer! (&optional (notice "Processing"))
      (dom-replace (by-selector ".footer .notice") (who-ps-html (:span :class "notice" notice)))
      (show! (by-selector ".footer")))

    (defun hide-footer! ()
      (hide! (by-selector ".footer")))
    
    (defun show-title-input () 
      (let ((input (by-selector ".book-title input")))
	(show! input)
	(show! (by-selector ".book-package"))
	(chain input (focus))
	(chain input (select))
	(hide! (by-selector ".book-title h1"))))

    (defun hide-title-input () 
      (hide! (by-selector ".book-title input"))
      (hide! (by-selector ".book-package"))
      (show! (by-selector ".book-title h1")))

    (defun notebook-package-template (package &optional result)
      (who-ps-html
       (:div :class "book-package"
	     (:textarea :onchange "repackageBook(this.value)" package)
	     (when result 
	       (who-ps-html 
		(:ul :class "result"
		     (:li :class "error" (condition-template result))))))))
    
    (defun notebook-title-template (name)
      (who-ps-html
       (:div :class "book-title"
	     (:input :class "text" :onchange "renameBook(this.value)" :value name)
	     (:h1 :onclick "showTitleInput()" name))))

    (defun notebook-template (notebook)
      (+ (notebook-title-template (notebook-name notebook))
	 (notebook-package-template (notebook-package notebook))
	 (who-ps-html
	  (:ul :class "cells"
	       (join (map (lambda (cell) (cell-template cell))
			  (notebook-cells notebook)))))))))

(define-handler (js/ajax.js :content-type "application/javascript") ()
  (ps (defun kill-thread ()
	(post/json "/cl-notebook/system/kill-thread" (create)))
      
      (defun arg-hint (symbol x y &key current-arg)
	(post/json "/cl-notebook/system/arg-hint" (create :name symbol :package :cl-notebook)
		   (lambda (res)
		     (unless (and (@ res error) (= (@ res error) 'function-not-found))
		       (dom-append 
			(by-selector "body") 
			(who-ps-html 
			 (:div :class "notebook-arg-hint cm-s-default" 
			       :style (+ "left:" x "px; top: " y "px")
			       "(" (:span :class "name" symbol)
			       (join (loop for arg in (@ res args)
					collect (cond 
						  ((and (string? arg) (= (@ arg 0) "&"))
						   (who-ps-html (:span :class "modifier cm-variable-2" arg)))
						  ((string? arg)
						   (who-ps-html (:span :class "arg" arg)))
						  (t
						   (who-ps-html (:span :class "compound-arg" 
								       "(" (join 
									    (loop for a in arg
									       if (null a) 
									       collect (who-ps-html (:span :class "arg" "NIL"))
									       else collect (who-ps-html (:span :class "arg" a)))) ")")))))) ")")))))))
      
      (defun rewind-book (index)
	(post/json "/cl-notebook/notebook/rewind" (create :book (notebook-id *notebook*) :index index)
		   #'notebook!))

      (defvar debounced-rewind (debounce #'rewind-book 10))

      (defun fork-book (callback)
	(post/json "/cl-notebook/notebook/fork-at" (create :book (notebook-id *notebook*) :index (@ (by-selector "#book-history-slider") value))
		   callback))

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
	(post/json "/cl-notebook/notebook/kill" (create :book (notebook-id *notebook*))))

      (defun rename-book (new-name)
	(post/fork "/cl-notebook/notebook/rename" (create :book (notebook-id *notebook*) :new-name new-name)))
      
      (defun repackage-book (new-package)
	(post/fork "/cl-notebook/notebook/repackage" (create :book (notebook-id *notebook*) :new-package new-package)))

      (defun notebook/eval-to-cell (cell-id contents)
	(post/fork "/cl-notebook/notebook/eval-to-cell" (create :book (notebook-id *notebook*) :cell-id cell-id :contents contents)))

      (defun new-cell (&optional (cell-language :common-lisp) (cell-type :code))
	(post/fork "/cl-notebook/notebook/new-cell" (create :book (notebook-id *notebook*) :cell-type cell-type :cell-language cell-language)))

      (defun kill-cell (cell-id)
	(post/fork "/cl-notebook/notebook/kill-cell" (create :book (notebook-id *notebook*) :cell-id cell-id)))
      
      (defun change-cell-contents (cell-id contents)
	(post/fork "/cl-notebook/notebook/change-cell-contents" (create :book (notebook-id *notebook*) :cell-id cell-id :contents contents)))

      (defun change-cell-type (cell-id new-type)
	(post/fork "/cl-notebook/notebook/change-cell-type" (create :book (notebook-id *notebook*) :cell-id cell-id :new-type new-type)))
      
      (defun change-cell-language (cell-id new-language)
	(post/fork "/cl-notebook/notebook/change-cell-language" (create :book (notebook-id *notebook*) :cell-id cell-id :new-language new-language)))

      (defun change-cell-noise (cell-id new-noise)
	(post/fork "/cl-notebook/notebook/change-cell-noise" (create :book (notebook-id *notebook*) :cell-id cell-id :new-noise new-noise)))

      (defun reorder-cells (ev)
	(prevent ev)
	(let ((ord (obj->string
		    (loop for elem in (by-selector-all ".cell")
		       collect (parse-int (chain elem (get-attribute :cell-id)))))))
	  (post "/cl-notebook/notebook/reorder-cells" 
		(create :book (notebook-id *notebook*) 
			:cell-order ord))))
      
      (defun system/macroexpand-1 (expression callback)
	(post "/cl-notebook/system/macroexpand-1" 
	      (create :expression expression)
	      callback))
      (defun system/macroexpand (expression callback)
	(post "/cl-notebook/system/macroexpand"
	      (create :expression expression)
	      callback))))

(define-handler (js/book-actions.js :content-type "application/javascript") ()
  (ps 
    (defvar *book-actions*
      (create :export-html
	      (lambda ()
		(save-file
		 (+ (notebook-name *notebook*) ".html")
		 (+ (who-ps-html 
		     (:style :type "text/css" :media "screen"
			     (join
			      (list "<!--"
				    (sheet-text 2 (matching? "^.cm-s-default"))
				    (sheet-text 0 (lambda (text)
						    (regex-match-any 
						     text
						     "^.result" "^.stdout"
						     "^.warnings" "^.condition-contents"
						     "^.chart")))
				    "-->")
			      #\newline))
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

(define-handler (js/pareditesque.js :content-type "application/javascript") ()
  (ps
    ;;;;;;;;;; Utility for s-exp navigation
    ;;;;;;;;;;;;;;;
    (defun matching-brace (brace)
      (aref 
       (create "(" ")" ")" "("
	       "[" "]" "]" "["
	       "{" "}" "}" "{"
	       "<" ">" ">" "<")
       brace))

    ;;;;;;;;;; Basic codemirror-related predicates and getters
    ;;;;;;;;;;;;;;;
    ;; TODO fix and test these predicates
    (defun at-beginning? (mirror &key (ls (lines (mirror-contents mirror))))
      (with-slots (line ch) (get-cur :right mirror)
	  (and (>= 0 line) (>= 0 ch))))
    (defun at-end? (mirror &key (ls (lines (mirror-contents mirror))))
      (with-slots (line ch) (get-cur :right mirror)
	(and (>= line (- (length ls) 1))
	     ;; the line length doesn't include newline
	     ;; so we don't subtract one here
	     (>= ch (length (last ls))))))
    (defun at-empty-line? (mirror &key (ls (lines (mirror-contents mirror))))
      (with-slots (line ch) (get-cur :right mirror)
	(= 0 (length (aref ls line)))))

    (defun cur-compare (a b)
      (cond ((and (= (@ a line) (@ b line)) (= (@ a ch) (@ b ch)))
	     :eq)
	    ((= (@ a line) (@ b line))
	     (if (> (@ a ch) (@ b ch))
		 :gt
		 :lt))
	    ((> (@ a line) (@ b line)) :gt)
	    (t :lt)))

    (defun mirror-contents (mirror)
      (chain mirror (get-value)))

    (defun get-cur (direction mirror &optional (component :head))
      (with-slots (line ch) (chain mirror (get-cursor component))
	(create :line line :ch (if (= direction :left) (- ch 1) ch))))

    (defun token-type-at-cursor (direction mirror)
      (with-slots (line ch) (get-cur direction mirror)
	(chain mirror (get-token-type-at (create :line line :ch (+ 1 ch))))))
    (defun token-type-at-cursor? (direction mirror type)
      (= type (token-type-at-cursor direction mirror)))
    (defun string-at-cursor? (direction mirror)
      (token-type-at-cursor? direction mirror :string))
    (defun bracket-at-cursor? (direction mirror)
      (token-type-at-cursor? direction mirror :bracket))    

    (defun go-char (direction mirror)
      (chain mirror (exec-command
		     (case direction
		       (:left "goCharLeft")
		       (:right "goCharRight")))))

    (defun go-line (direction mirror)
      (chain mirror (exec-command
		     (case direction
		       (:up "goLineUp")
		       (:down "goLineDown")))))
    
    (defun char-at-cursor (direction mirror &key (ls (lines (mirror-contents mirror))))
      (with-slots (line ch) (get-cur direction mirror)
	(aref ls line ch)))
    (defun char-at-cursor? (direction mirror char &key (ls (lines (mirror-contents mirror))))
      (= char (char-at-cursor direction mirror :ls ls)))

    ;;;;;;;;;; Character skipping for code-mirror contents
    ;;;;;;;;;;;;;;;
    (defun skip-while (fn mirror direction &key (ls (lines (mirror-contents mirror))))
      (let ((til (case direction
		   (:left #'at-beginning?)
		   (:right #'at-end?))))
	(loop do (go-char direction mirror) until (til mirror ls)
	   while (fn (char-at-cursor direction mirror :ls ls)))))
    
    (defun skip-until (fn mirror direction &key (ls (lines (mirror-contents mirror))))
      (let ((til (case direction
		   (:left #'at-beginning?)
		   (:right #'at-end?))))
	(loop do (go-char direction mirror) until (til mirror ls)
	   until (fn (char-at-cursor direction mirror :ls ls)))))

    (defun skip-to (direction mirror chars &key (ls (lines (mirror-contents mirror))))
      (let ((s (new (-set chars))))
	(skip-until (lambda (char) (chain s (has char))) mirror direction :ls ls)))
    (defun skip-over (direction mirror chars &key (ls (lines (mirror-contents mirror))))
      (let ((s (new (-set chars))))
	(skip-while (lambda (char) (chain s (has char))) mirror direction :ls ls)))
    (defun skip-whitespace (direction mirror &key (ls (lines (mirror-contents mirror))))
      (skip-over direction mirror (list #\space #\newline #\tab undefined) :ls ls))

    ;;;;;;;;;; block navigation
    ;;;;;;;;;;;;;;;
    (defun go-block (direction mirror)
      (let ((til (case direction
      		   (:up #'at-beginning?)
      		   (:down #'at-end?)))
      	    (ls (lines (mirror-contents mirror))))
	(when (at-empty-line? mirror :ls ls) (go-line direction mirror))
      	(loop until (or (til mirror :ls ls) (at-empty-line? mirror :ls ls))
      	   do (go-line direction mirror))))

    (defun select-block (direction mirror)
      (let ((start (get-cur direction mirror)))
	(go-block direction mirror)
	(chain mirror (extend-selection (get-cur direction mirror) start))))

    ;;;;;;;;;; s-exp navigation
    ;;;;;;;;;;;;;;;   
    (defun go-sexp (direction mirror)
      (destructuring-bind (paren til)
	  (case direction
	    (:right (list "(" #'at-end?))
	    (:left (list ")" #'at-beginning?)))
	(let ((ls (lines (mirror-contents mirror)))
	      (other-paren (matching-brace paren)))
	  (skip-whitespace direction mirror :ls ls)
	  (cond ((and (string-at-cursor? direction mirror) (not (char-at-cursor? direction mirror "\"" :ls ls)))
		 (skip-to direction mirror (list " " "\"" undefined) :ls ls))
		((string-at-cursor? direction mirror)
		 (skip-until 
		  (lambda (c) (not (string-at-cursor? direction mirror)))
		  mirror direction :ls ls))
		((token-type-at-cursor? direction mirror :comment)
		 (skip-to direction mirror (list " " undefined) :ls ls))
		((and (bracket-at-cursor? direction mirror)
		      (char-at-cursor? direction mirror other-paren :ls ls))
		 (go-char direction mirror))
		((and (bracket-at-cursor? direction mirror)
		      (char-at-cursor? direction mirror paren :ls ls))
		 (loop with tally = 1 until (til mirror :ls ls)
		    do (go-char direction mirror)
		    when (and (char-at-cursor? direction mirror paren :ls ls) 
			      (not (string-at-cursor? direction mirror))) 
		      do (incf tally)
		    when (and (char-at-cursor? direction mirror other-paren :ls ls)
			      (not (string-at-cursor? direction mirror)))
		      do (decf tally)
		    until (and (char-at-cursor? direction mirror other-paren :ls ls) (= 0 tally)))
		 (go-char direction mirror))
		(t 
		 (skip-to direction mirror (+ " " other-paren) :ls ls))))))

    (defun select-sexp (direction mirror)
      (destructuring-bind (desired opposite)
	  (case direction
	    (:left (list :gt :right))
	    (:right (list :lt :left)))
	(let ((start (get-cur :right mirror :anchor))
	      (end (get-cur :right mirror)))
	  (go-sexp direction mirror)
	  (if (and (chain mirror (something-selected))
		   (= desired (cur-compare start end)))
	      (chain mirror (extend-selection end))
	      (chain mirror (set-selection start (get-cur :right mirror)))))))

    (defun kill-sexp (direction mirror)
      (let ((start (get-cur direction mirror)))
	(go-sexp direction mirror)
	(chain mirror (replace-range "" start (get-cur :right mirror)))))

    ;;;;;;;;;; s-exp extras
    (defun sexp-at-point (direction mirror)
      (let ((start (get-cur direction mirror)))
	(go-sexp direction mirror)
	(let ((res (chain mirror (get-range start (get-cur :right mirror)))))
	  (chain mirror (set-cursor start))
	  res)))
    (defun slurp-sexp (direction mirror)
      (console.log "TODO -- slurp-sexp"))
    (defun barf-sexp (direction mirror) 
      (console.log "TODO -- barf-sexp"))
    (defun transpose-sexp (direction mirror)
      (console.log "TODO -- transpose-sexp"))
    (defun toggle-comment-region (mirror) 
      (let ((anchor (get-cur :right mirror :anchor))
	    (head (get-cur :right mirror :head)))
	(destructuring-bind (from to) (if (> (@ anchor line) (@ head line)) (list head anchor) (list anchor head))
	  (if (token-type-at-cursor? :right mirror :comment)
	      (chain mirror (uncomment from to))
	      (chain mirror (line-comment from to))))))
    
    ;;;;;;;;;; cell navigation
    (defun go-cell (direction cell-id)
      (let* ((cell (by-cell-id cell-id))
	     (next (case direction
		     (:down (@ cell next-sibling))
		     (:up (@ cell previous-sibling)))))
	(when next
	  (scroll-to-elem next)
	  (show-editor (elem->cell-id next)))))

    (defun transpose-cell! (direction cell-id)
      (let* ((cell (by-cell-id cell-id))
	     (next (case direction
		     (:down (@ cell next-sibling))
		     (:up (@ cell previous-sibling)))))
	(when next
	  (cond ((equal :up direction)
		 (chain next parent-node (insert-before cell next)))
		((and (equal :down direction) (@ next next-sibling))
		 (chain next parent-node (insert-before cell (@ next next-sibling))))
		(t
		 (chain next parent-node (append-child cell))))
	  (reorder-cells nil)
	  (scroll-to-elem cell)
	  (show-editor cell-id))))))

(define-handler (js/main.js :content-type "application/javascript") ()
  (ps
    ;; cl-notebook specific utility
    (defun by-cell-id (cell-id &rest children)
      (by-selector
       (+ "#cell-" cell-id
	  (if (> (length children) 0) " " "")
	  (join children " "))))

    (defun elem->cell-id (elem)
      (parse-int
       (chain elem (get-attribute "id") 
	      (match (-reg-exp "cell-([1234567890]+)"))
	      1)))

    (defun elem-to-cell (elem)
      (aref *notebook* :objects (elem->cell-id elem)))

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

    (defun in-present? ()
      (let ((slider (by-selector "#book-history-slider")))
	(= (@ slider value) (chain slider (get-attribute :max)))))

    (defun post/fork (uri args on-success on-fail) 
      (if (in-present?)
	  (post/json uri args on-success on-fail)
	  (fork-book (lambda (res)
		       (surgical! res)
		       (setf document.title (+ (@ res book-name) " - cl-notebook"))
		       (setf (@ args :book) (@ res id))
		       (dom-replace (by-selector ".book-title") (notebook-title-template (@ res book-name)))
		       (post/json uri args on-success on-fail)))))
    
    ;; cl-notebook specific DOM manipulation
    (defun display-book (book-name)
      (when book-name
	(set-page-hash (create :book book-name))
	(hash-updated)))

    (defun hash-updated ()
      (let ((book-name (@ (get-page-hash) :book)))
	(when book-name
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
      (setup-cell-mirror! cell))

    ;; CodeMirror and utilities
    (defun register-helpers (type object)
      (map 
       (lambda (fn name)
	 (chain -code-mirror
		(register-helper type name fn)))
       object))
    
    (defun register-commands (object)
      (map 
       (lambda (fn name)
	 (setf (aref -code-mirror :commands name) fn))
       object))
    
    (defun show-editor (cell-id)
      (show! (by-cell-id cell-id ".CodeMirror"))
      (chain (cell-mirror cell-id) (focus)))

    (defun hide-editor (cell-id)
      (hide! (by-cell-id cell-id ".CodeMirror")))

    (defun cell-mirror (cell-id)
      (@ (notebook-cell *notebook* cell-id) editor))

    (defun cell-editor-contents (cell-id)
      (chain (cell-mirror cell-id) (get-value)))

    (defun mirror! (text-area &key (extra-keys (create)))
      (let ((options 
	     (create
	      "async" t
	      "lineNumbers" t
	      "matchBrackets" t
	      "autoCloseBrackets" t
	      "lineWrapping" t
	      "viewportMargin" -infinity
	      "smartIndent" t
	      "extraKeys" (extend
			   (create "Ctrl-Space" 'autocomplete
				   
				   "Ctrl-Right" (lambda (mirror) (go-sexp :right mirror))
				   "Ctrl-Left" (lambda (mirror) (go-sexp :left mirror))
				   "Shift-Ctrl-Right" (lambda (mirror) (select-sexp :right mirror))
				   "Shift-Ctrl-Left" (lambda (mirror) (select-sexp :left mirror))

				   "Ctrl-Down" (lambda (mirror) (go-block :down mirror))
				   "Shift-Ctrl-Down" (lambda (mirror) (select-block :down mirror))
				   "Ctrl-Up" (lambda (mirror) (go-block :up mirror))
				   "Shift-Ctrl-Up" (lambda (mirror) (select-block :up mirror))
				   
				   "Ctrl-Alt-K" (lambda (mirror) (kill-sexp :right mirror))
				   "Shift-Ctrl-Alt-K" (lambda (mirror) (kill-sexp :left mirror))
				   "Tab" 'indent-auto

				   "Ctrl-;" (lambda (mirror) (toggle-comment-region mirror)))
			   extra-keys))))
	(chain -code-mirror (from-text-area text-area options))))

    (defun setup-cell-mirror! (cell)
      (let* ((cell-id (@ cell id))
	     (mirror (mirror! (by-cell-id cell-id ".cell-contents")
			      :extra-keys (create
					   "Ctrl-Enter"
					   (lambda (mirror)
					     (let ((contents (cell-editor-contents cell-id)))
					       (notebook/eval-to-cell cell-id contents)))
					   
					   "Ctrl-]" (lambda (mirror) (go-cell :down cell-id))
					   "Ctrl-[" (lambda (mirror) (go-cell :up cell-id))
					   "Shift-Ctrl-]" (lambda (mirror) (transpose-cell! :down cell-id))
					   "Shift-Ctrl-[" (lambda (mirror) (transpose-cell! :up cell-id))
					   "Ctrl-E" (lambda (mirror) 
						      (system/macroexpand-1 
						       (sexp-at-point :right mirror)
						       (lambda (res)
							 (chain *macro-expansion-mirror*
								(set-value res)))))))))
	(setf (@ cell editor) mirror)
	(chain mirror (on 'cursor-activity
			  (lambda (mirror)
			    (unless (chain mirror (something-selected))
			      (chain mirror (exec-command 'show-arg-hint))))))
	(unless (= (@ cell type) "markup")
	  (chain mirror (on 'change
			    (lambda (mirror change)
			      (when (or (= "+input" (@ change origin)) (= "+delete" (@ change origin)))
				(chain -code-mirror commands 
				       (autocomplete 
					mirror (@ -code-mirror hint ajax)
					(create :async t "completeSingle" false))))))))
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
    (defun notebook-package (notebook) (@ notebook package))
    (defun notebook-id (notebook) (@ notebook id))
    (defun set-notebook-name (notebook new-name) (setf (@ notebook name) new-name))
    (defun set-notebook-package (notebook new-package) (setf (@ notebook package) new-package))

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
    
    (defun setup-package-mirror! ()
      (mirror! 
       (by-selector ".book-package textarea")
       :extra-keys
       (create "Ctrl-]"     (lambda (mirror) 
			      (let ((next (by-selector ".cells .cell")))
				(when next
				  (hide-title-input)
				  (scroll-to-elem next)
				  (show-editor (elem->cell-id next)))))
	       "Ctrl-Enter" (lambda (mirror)
			      (repackage-book (chain mirror (get-value))))))
      (hide! (by-selector ".book-package")))

    (defun setup-macro-expansion-mirror! ()
      (setf *macro-expansion-mirror* 
	    (mirror! 
	     (by-selector "#macro-expansion textarea")
	     :extra-keys
	     (create "Ctrl-E" (lambda (mirror) 
				(system/macroexpand-1 
				 (sexp-at-point :right mirror)
				 (lambda (res)
				   (chain *macro-expansion-mirror*
					  (set-value res)))))))))

    (defun surgical! (raw)
      (let* ((slider (by-selector "#book-history-slider"))		     
	     (count (@ raw history-size))
	     (pos (or (@ raw history-position) count))
	     (id (@ raw id)))
	(chain slider (set-attribute :max count))
	(setf (@ *notebook* id) id
	      (@ slider value) pos
	      (@ (by-selector "#book-history-text") value) pos)
	(hide! (by-selector ".book-title input"))
	(setup-package-mirror!)
	(set-page-hash (create :book id))))

    (defun notebook! (raw)
      (let* ((fs (@ raw facts)))
	(setf *notebook* 
	      (create :facts fs :objects (notebook-condense fs)
		      :history-size (@ raw history-size)
		      :id (@ raw :id) 
		      :name (loop for (a b c) in fs
			       when (equal b "notebookName")
			       do (return c))
		      :package (loop for (a b c) in fs
				  when (equal b "notebookPackage")
				  do (return c))))
	(dom-set 
	 (by-selector "#notebook")
	 (notebook-template *notebook*))
	(surgical! raw)
	(setf document.title (+ (notebook-name *notebook*) " - cl-notebook"))
	(nativesortable (by-selector "ul.cells"))
	(map (lambda (opt) (chain opt (remove-attribute :selected)))
	     (by-selector-all "#book-list option"))
	(chain (by-selector (+ "#book-list option[value='" (notebook-id *notebook*) "']"))
	       (set-attribute :selected "selected"))
	(map (lambda (cell) 
	       (with-slots (id cell-type) cell
		 (setup-cell-mirror! cell)
		 (when (= 'markup cell-type)
		   (hide! (by-cell-id id ".CodeMirror")))))
	     (notebook-cells *notebook*))))

    (defun relevant-event? (ev)
      (and (in-present?) (equal (notebook-id *notebook*) (@ ev book))))

    (defun notebook-events ()
      (event-source 
       "/cl-notebook/source"
       (create
	'new-cell 
	(lambda (res)
	  (when (relevant-event? res)
	    (let ((id (@ res 'cell-id))
		  (cell (create 'type "cell" 'contents "" 'result ""
				'cell-type (@ res cell-type) 
				'cell-language (@ res cell-language)
				'id (@ res 'cell-id))))
	      (setf (aref (notebook-objects *notebook*) id) cell)
	      (chain (notebook-facts *notebook*) (unshift (list id 'cell nil)))
	      (dom-append (by-selector ".cells")
			  (cell-template cell))
	      (setup-cell-mirror! cell)
	      (scroll-to-elem (by-cell-id id))
	      (show-editor id))))
	'change-cell-type
	(lambda (res)
	  (when (relevant-event? res)
	    (let ((cell (notebook-cell *notebook* (@ res cell))))
	      (setf (@ cell cell-type) (@ res new-type))
	      (dom-replace-cell cell))))
	'change-cell-language
	(lambda (res)
	  (when (relevant-event? res)
	    (let ((cell (notebook-cell *notebook* (@ res cell))))
	      (setf (@ cell cell-language) (@ res new-language))
	      (dom-replace-cell cell))))
	'change-cell-noise
	(lambda (res)
	  (when (relevant-event? res)
	    (let ((cell (notebook-cell *notebook* (@ res cell))))
	      (setf (@ cell noise) (@ res new-noise))
	      (dom-replace-cell-value cell))))
	'starting-eval
	(lambda (res) (show-footer!))
	'killed-eval
	(lambda (res) (hide-footer!))
	'finished-eval 
	(lambda (res)
	  (hide-footer!)
	  (when (relevant-event? res)
	    (let ((cell (notebook-cell *notebook* (@ res cell))))
	      (setf (@ cell contents) (@ res contents)
		    (@ cell result) (@ res result))
	      (delete (@ cell stale))
	      (chain (by-cell-id (@ res cell)) class-list (remove "stale"))
	      (dom-replace-cell-value cell))))
	'finished-package-eval
	(lambda (res)
	  (hide-footer!)
	  (let ((id (@ res book))
		(new-package (@ res contents))
		(err (@ res result)))
	    (when (relevant-event? res)
	      (dom-replace (by-selector ".book-package") (notebook-package-template new-package err))
	      (set-notebook-package *notebook* new-package)
	      (setup-package-mirror!)
	      (if err
		  (show-title-input)
		  (hide-title-input)))))

	'loading-package
	(lambda (res) (show-footer! (+ "Loading package '" (@ res package) "'")))
	'finished-loading-package
	(lambda (res) (hide-footer!))
	'package-load-failed
	(lambda (res) (hide-footer!))

	'content-changed
	(lambda (res)
	  (when (relevant-event? res)
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
	  (when (relevant-event? res)
	    (delete (aref *notebook* 'objects (@ res cell)))
	    (chain (by-cell-id (@ res cell)) (remove))))
	
	'reorder-cells 
	(lambda (res)
	  (when (relevant-event? res)
	    ;; TODO change order here to support proper multi-user noting
	    (console.log "Changed cell order" res)))
	
	'new-book
	(lambda (res)
	  (let ((id (@ res book))
		(name (@ res book-name)))
	    (dom-append (by-selector "#book-list")
			(who-ps-html (:option :value id name)))))
	'kill-book
	(lambda (res)
	  (let ((id (@ res book)))
	    (chain (by-selector (+ "#book-list option[value='" id "']")) (remove))
	    (when (equal (notebook-id *notebook*) id) ;; intentionally don't call relevant-event? here.
	      ;; TODO. If someone else deletes the book you're editing, this could get confusing.
	      ;;       Maybe put up a notice of "Book deleted" instead of moving on to some arbitrary "first book"?
	      (display-book 
	       (chain (@ (by-selector-all "#book-list option") 1)
		      (get-attribute :value))))))
	
	'rename-book
	(lambda (res)
	  (let ((id (@ res book))
		(new-name (@ res new-name)))
	    (when (relevant-event? res)
	      (dom-replace (by-selector ".book-title") (notebook-title-template new-name))
	      (set-notebook-name *notebook* new-name)
	      (hide-title-input))
	    (chain (by-selector (+ "#book-list option[value='" id "']")) (remove))
	    (dom-append (by-selector "#book-list")
			(who-ps-html (:option :value id new-name))))))))

    (defvar *warning-filter* 
      (lambda (w)
	(or (chain (@ w condition-type) (starts-with "REDEFINITION"))
	    (and (@ w error-message)
		 (or  (chain (@ w error-message) (starts-with "undefined "))
		      (chain (@ w error-message) (ends-with "never used.")))))))

    (dom-ready
     (lambda ()
       ;;; Setting up some custom CodeMirror code ;;;;;;;;;;;;;;;;;;;;
       (register-helpers 
	"hint"
	(create :ajax
		(lambda (mirror callback options)
		  (let* ((cur (chain mirror (get-cursor)))
			 (tok (chain mirror (get-token-at cur))))
		    (when (> (length (@ tok string)) 2)
		      (get "/cl-notebook/system/complete" (create :partial (@ tok string) :package :cl-notebook)
			   (lambda (res)
			     (callback 
			      (create :list (or (string->obj res) (new (-array)))
				      :from (chain -code-mirror (-pos (@ cur line) (@ tok start)))
				      :to (chain -code-mirror (-pos (@ cur line) (@ tok end))))))))))
		:auto
		(lambda (mirror options)
		  (chain -code-mirror commands 
			 (autocomplete mirror (@ -code-mirror hint ajax) (create :async t))))))

       (register-commands
	(create show-arg-hint
		(debounce
		 (lambda (mirror)
		   ($aif (by-selector-all ".notebook-arg-hint")
			 (map (lambda (elem) (chain elem (remove))) it))
		   (labels ((find-first (ctx) 
			      (cond ((null ctx) nil)
				    ((or (= "arglist" (@ ctx node_type))
					 (and (@ ctx prev)
					      (@ ctx prev node_type)
					      (= "arglist" (@ ctx prev node_type)))) nil)
				    ((= "(" (@ ctx opening)) (@ ctx first))
				    (t (find-first (@ ctx prev))))))
		     (let* ((coords (chain mirror (cursor-coords)))
			    (cur (chain mirror (get-cursor)))
			    (tok (chain mirror (get-token-at cur))))
		       ($aif (and tok (find-first (@ tok state ctx)))
			     (arg-hint it (+ 1 (@ coords right)) (@ coords bottom))))))
		 100)))
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (notebook-events)
       (hide-footer!)
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
       (setup-macro-expansion-mirror!)
       (setf (@ window onhashchange) #'hash-updated)
       (hash-updated)))))
