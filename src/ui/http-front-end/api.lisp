(in-package :cl-notebook)

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
		     (dom-set
		      (by-selector "#notebook")
		      (who-ps-html (:h2 "Notebook '" name "' not found..."))))))

      (defun new-book ()
        (post/json "/cl-notebook/notebook/new" (create) #'notebook!))

      (defun load-book (path)
        (post/json "/cl-notebook/notebook/load" (create :path path) #'notebook!))

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
    (defun export-html ()
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

    (defun export-lisp ()
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

    (defvar *book-actions*
      (create :export-html #'export-html
	      :export-lisp #'export-lisp))

    (defun run-book-action (action)
      (setf (@ (by-selector "#book-actions") selected-index) 0)
      ($aif (aref *book-actions* action)
	    (funcall it)
	    (console.log "NOT YET IMPLEMENTED: " action)))))
