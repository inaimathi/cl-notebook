(in-package :cl-notebook)

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
	(loop until (til mirror ls) while (fn (char-at-cursor direction mirror :ls ls))
	   do (go-char direction mirror))))

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
		((token-type-at-cursor? direction mirror :quote-char)
		 (skip-until
		  (lambda (c) (not (token-type-at-cursor? direction mirror :quote-char)))
		  mirror direction :ls ls)
		 (go-sexp direction mirror))
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
      (replace-sexp-at-point direction mirror ""))

    ;;;;;;;;;; s-exp extras
    (defun sexp-at-point (direction mirror)
      (let ((start (get-cur direction mirror)))
	(go-sexp direction mirror)
	(let ((res (chain mirror (get-range start (get-cur :right mirror)))))
	  (chain mirror (set-cursor start))
	  res)))
    (defun replace-sexp-at-point (direction mirror new-sexp)
      (let ((start (get-cur direction mirror)))
	(go-sexp direction mirror)
	(chain mirror (replace-range new-sexp start (get-cur :right mirror)))))
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
