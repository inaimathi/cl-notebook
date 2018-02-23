(in-package :cl-notebook)

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

    (defun show-macro-expansion! ()
      (show! (by-selector "#macro-expansion")))

    (defun hide-macro-expansion! ()
      (hide! (by-selector "#macro-expansion")))

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
