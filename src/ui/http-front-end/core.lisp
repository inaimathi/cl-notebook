(in-package :cl-notebook)

(define-handler (/) ()
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
      (:script :type "text/javascript" :src "/js/core.js")
      (:script :type "text/javascript" :src "/js/pareditesque.js")
      (:script :type "text/javascript" :src "/js/notebook-selector.js")
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
            (:span :id "notebook-selector")
	    (:button :onclick "newCell()" "+ New Cell")
	    (:button :onclick "newBook()" "+ New Book")
	    (:select :id "book-list"
		     :onchange "displayBook(this.value)"
		     (:option :value "" "Choose book...")
		     (loop for (id name) in (loaded-books)
		     	do (htm (:option :value id (str id)))))
	    (:select :id "book-actions"
		     :onchange "runBookAction(this.value)"
		     (:option :value "" "Stuff...")
		     (:optgroup
		      :label "Export"
		      (:option :value "export-html" "Export as HTML")
		      (:option :value "export-lisp" "Export as .lisp"))))
      (:div :id "macro-expansion" (:textarea :language "commonlisp"))
      (:div :id "notebook")
      (:div :class "footer"
	    (:span :class "notice" "Processing")
	    (:img :src "/static/img/dots.png")
	    (:button :onclick "killThread()" :class "right" "! Abort"))))))

(define-handler (js/core.js :content-type "application/javascript") ()
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

    (defun mirror! (text-area &key (extra-keys (create)) (line-wrapping? t))
      (let ((options
	     (create
	      "async" t
	      "lineNumbers" t
	      "matchBrackets" t
	      "autoCloseBrackets" t
	      "lineWrapping" line-wrapping?
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
					   "Shift-Ctrl-E" (lambda (mirror)
							    (show-macro-expansion!)
							    (chain *macro-expansion-mirror* (focus)))
					   "Ctrl-E" (lambda (mirror)
						      (system/macroexpand-1
						       (sexp-at-point :right mirror)
						       (lambda (res)
							 (show-macro-expansion!)
							 (chain *macro-expansion-mirror*
								(set-value res)))))
					   "Ctrl-Space" (lambda (mirror)
							  (console.log "TOKEN: " (token-type-at-cursor :right mirror)))))))
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
	     :line-wrapping? nil
	     :extra-keys
	     (create "Ctrl-E" (lambda (mirror)
				(system/macroexpand-1
				 (sexp-at-point :right mirror)
				 (lambda (res)
				   (chain
				    mirror
				    (operation
				     (lambda ()
				       (let ((start (get-cur :right mirror)))
					 (replace-sexp-at-point :right mirror res)
					 (chain mirror (set-selection start (get-cur :right mirror)))
					 (chain mirror (exec-command 'indent-auto))
					 (chain mirror (set-cursor start)))))))))))))

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
	  (let ((id (@ res book)))
	    (dom-append (by-selector "#book-list")
			(who-ps-html (:option :value id id)))))

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
	    (chain (@ w condition-type) (starts-with "IMPLICIT-GENERIC-"))
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
       (hide-macro-expansion!)
       (chain
	(by-selector "body")
	(add-event-listener
	 :keyup (key-listener
		 <esc> (progn
			 (clear-selection)
			 (hide-title-input)
			 (hide-macro-expansion!)
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
