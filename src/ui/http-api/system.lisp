(in-package #:cl-notebook)

;;;;;;;;;; System-level hooks
(define-json-handler (cl-notebook/system/kill-thread) ()
  (when (and (bt:threadp *front-end-eval-thread*)
	     (bt:thread-alive-p *front-end-eval-thread*))
    (bt:destroy-thread *front-end-eval-thread*))
  (publish-update! nil 'killed-eval)
  :ok)

(define-json-handler (cl-notebook/system/formats) ()
  (hash :eval (front-end-eval-formats)
        :export (export-book-formats)))

(define-json-handler (cl-notebook/system/home-path) ()
  (namestring (user-homedir-pathname)))

(define-json-handler (cl-notebook/system/ls) ((dir :existing-directory))
  (let ((dirs nil)
        (files nil))
    (loop for f in (cl-fad:list-directory dir)
       do (let* ((dir (pathname-directory f))
                 (p (hash :string (namestring f)
                          :path-type (first dir)
                          :directory (rest dir)
                          :name (pathname-name f))))
            (if (cl-fad:directory-pathname-p f)
                (push p dirs)
                (push p files))))
    (hash :files (nreverse files) :directories (nreverse dirs))))

(define-handler (cl-notebook/source :close-socket? nil) ()
  (subscribe! :cl-notebook-updates sock))

(define-json-handler (cl-notebook/loaded-books) ()
  (mapcar
   (lambda (book)
     (hash :path (car book) :title (notebook-name (second book))))
   (loaded-books!)))

;;;;;;;;;; Server-side hint hooks
(define-json-handler (cl-notebook/system/complete) ((partial :string) (package :keyword))
  (let ((p (string-upcase partial))
	(res))
    (do-symbols (s package)
      (when (alexandria:starts-with-subseq p (symbol-name s))
	(push s res)))
    (sort (mapcar (lambda (s) (string-downcase (symbol-name s)))
		  (remove-duplicates res))
	  #'< :key #'length)))

(define-handler (cl-notebook/system/macroexpand-1 :content-type "plain/text") ((expression :string))
  (format nil "~s" (macroexpand-1 (read-from-string expression))))

(define-handler (cl-notebook/system/macroexpand :content-type "plain/text") ((expression :string))
  (format nil "~s" (macroexpand (read-from-string expression))))

(define-json-handler (cl-notebook/system/arg-hint) ((name :string) (package :keyword))
  (multiple-value-bind (sym-name fresh?) (intern (string-upcase name) package)
    (if (fboundp sym-name)
	(hash :args (labels ((->names (thing)
			       (typecase thing
				 (list (case (car thing)
					 (&environment (->names (cddr thing)))
					 (quote
					  (if (cddr thing)
					      (->names (cdr thing))
					      (concatenate 'string "'" (->names (cadr thing)))))
					 (t (mapcar #'->names thing))))
				 (symbol (string-downcase (symbol-name thing)))
				 (t thing))))
		      (->names (arglist sym-name))))
	(progn (when (not fresh?) (unintern sym-name))
	       (hash :error :function-not-found)))))
