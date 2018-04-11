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
                          :name (format nil "~a.~a" (pathname-name f) (pathname-type f)))))
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
(defun get-completions (partial package)
  (let* ((split (cl-ppcre:split "::?" partial))
         (match (cl-ppcre:scan-to-strings "::?" partial))
         (partial (string-downcase (or (second split) partial)))
         (package (if (cdr split)
                      (or (find-package (read-from-string (first split))) package)
                      package)))
    (when package
      (sort
       (remove-duplicates
        (if (string= ":" match)
            (loop for s being the external-symbols of package
               for n = (string-downcase (symbol-name s))
               when (alexandria:starts-with-subseq partial n) collect n)
            (loop for s being the symbols of package
               for n = (string-downcase (symbol-name s))
               when (alexandria:starts-with-subseq partial n) collect n)))
       #'< :key #'length))))

(define-json-handler (cl-notebook/system/complete) ((partial :string) (package :package))
  (get-completions partial package))

(define-handler (cl-notebook/system/macroexpand-1 :content-type "plain/text") ((expression :string))
  (format nil "~s" (macroexpand-1 (read-from-string expression))))

(define-handler (cl-notebook/system/macroexpand :content-type "plain/text") ((expression :string))
  (format nil "~s" (macroexpand (read-from-string expression))))

(define-json-handler (cl-notebook/system/arg-hint) ((name :string) (package :package))
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
