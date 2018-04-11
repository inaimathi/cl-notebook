(in-package :cl-notebook)

;;;;;;;;;; Notebook and constructor/destructor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass notebook (fact-base)
  ((namespace :accessor namespace :initform (find-package :cl-notebook) :initarg :namespace)))

(defun make-notebook (&key (file-name (make-unique-name-in *books* "new-book")))
  (make-instance
   'notebook :file-name file-name :in-memory? t
   :index (fact-base::make-index *default-indices*)
   :history (fact-base::queue)))

(defun new-notebook! (path)
  (let ((book (make-notebook :file-name path)))
    (insert-new! book :notebook-name (pathname-name path))
    (insert-new!
     book :notebook-package (default-package book))
    (setf (namespace book) (notebook-package! book))
    (register-notebook! book)
    book))

(defmethod load-notebook! ((file-name pathname))
  (assert (cl-fad:file-exists-p file-name) nil "Nonexistent file ~s" file-name)
  (let* ((book (make-notebook :file-name file-name)))
    (with-open-file (s file-name :direction :input)
      (loop for entry = (handler-case
                            (fact-base::read-entry! s)
                          (#+sbcl sb-int:simple-reader-package-error #-sbcl error (e)
                                  (load-package (slot-value e 'package))
                                  (fact-base::read-entry! s)))
         while entry
	 do (incf (fact-base::entry-count book))
	 do (fact-base::apply-entry! book entry)))
    (handler-bind (#+sbcl (sb-ext:name-conflict
			   (lambda (e)
			     (invoke-restart
                              (or (find-restart 'sb-impl::take-new e)
                                  (find-restart 'sb-impl::shadowing-import-it e))))))
      (setf (namespace book)
            (notebook-package! book))
      (eval-notebook book))
    (register-notebook! book)))

;;;;;;;;;; Notebook methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod new-cell! ((book notebook) &key (cell-language :common-lisp) (cell-type :code))
  (multi-insert! book `((:cell nil) (:cell-type ,cell-type) (:cell-language ,cell-language) (:contents "") (:result ""))))

(defmethod notebook-id ((book notebook))
  (namestring (file-name book)))

(defmethod default-package ((book notebook))
  (format nil "(defpackage ~s~%  (:use :cl :fact-base :cl-notebook))" (notebook-name book)))

(defmethod notebook-name ((book notebook))
  (caddar (lookup book :b :notebook-name)))

(defmethod notebook-cell-order ((book notebook))
  (let* ((ordered-ids (caddar (lookup book :b :cell-order)))
         (unordered-ids (set-difference
                         (reverse (mapcar #'car (lookup book :b :cell :c nil)))
                         ordered-ids)))
    (concatenate 'list ordered-ids unordered-ids)))

(defun reorder-cells! (book cell-order)
  (awhen (first (lookup book :b :cell-order))
    (delete! book it))
  (insert-new! book :cell-order cell-order)
  (write! book)
  book)

(defun notebook-cell (book cell-id)
  (cons (cons :id cell-id)
        (for-all `(,cell-id ?k ?v)
                 :collect (cons ?k ?v)
                 :in book)))

(defun map-cells (fn book)
  (loop for id in (notebook-cell-order book)
     collect (funcall fn (notebook-cell book id))))

(defun do-cells (fn book)
  (loop for id in (notebook-cell-order book)
     do (funcall fn (notebook-cell book id))))

(defmethod notebook-package-spec-string ((book notebook))
  (caddar (lookup book :b :notebook-package)))

(defmethod notebook-package-spec ((book notebook))
  (let ((default (default-package book)))
    (handler-case
	(or (read-from-string (notebook-package-spec-string book)) (read-from-string default))
      (error ()
	(read-from-string default)))))

(defmethod notebook-package! ((book notebook))
  ;; TODO handle loading and package-related errors here
  (let ((spec (notebook-package-spec book)))
    (load-dependencies! spec)
    (or (find-package (second spec)) (eval spec))))

(defun load-package (package)
  (unless (find-package package)
    (publish-update! nil 'loading-package :package package)
    (handler-bind ((error (lambda (e)
			    (publish-update! nil 'package-load-failed :package package :error (front-end-error nil e)))))
      (qlot:with-local-quicklisp (*storage*)
        (qlot/util:with-package-functions :ql (quickload)
          (quickload package)))
      (publish-update! nil 'finished-loading-package :package package))))

(defmethod load-dependencies! ((package-form list))
  (loop for exp in (cddr package-form)
     do (case (car exp)
	  (:use (mapc #'load-package (cdr exp)))
	  (:import-from (load-package (second exp)))
	  (:shadowing-import-from (load-package (second exp))))))

(defmethod repackage-notebook! ((book notebook) (new-package string))
  (let ((package-form (read-from-string new-package))
	(package-fact (first (lookup book :b :notebook-package)))
	(old-name (package-name (namespace book))))
    (handler-bind ((error (lambda (e)
			    (setf (namespace book) (rename-package (namespace book) old-name))
			    (insert! book (list (first package-fact) :package-error (front-end-error package-form e))))))
      (if (string= new-package (third package-fact))
	  (values book nil)
	  (progn
	    (setf (namespace book) (rename-package (namespace book) (second package-form)))
	    (load-dependencies! package-form)
	    (handler-bind (#+sbcl (sb-ext:name-conflict
				   (lambda (e)
                                     (invoke-restart
                                      (or (find-restart 'sb-impl::take-new e)
                                          (find-restart 'sb-impl::shadowing-import-it e))))))
	      (eval package-form))
	    (awhen (first (lookup book :b :package-error)) (delete! book it))
	    (if package-fact ;; TODO - remove conditional eventually. All notebooks should have such facts.
		(change! book package-fact (list (first package-fact) :notebook-package new-package))
		(insert-new! book :notebook-package new-package))
	    (values book t))))))

(defmethod rename-notebook! ((book notebook) (new-name string))
  "Takes a book and a new name.
Returns two values; the renamed book, and a boolean specifying whether the name was changed.
If the new name passed in is the same as the books' current name, we don't insert any new facts."
  (let* ((name-fact (first (lookup book :b :notebook-name)))
	 (same? (equal (third name-fact) new-name)))
    (unless same?
      (change! book name-fact (list (first name-fact) :notebook-name new-name)))
    (values book (not same?))))

(defun fork-at! (book index)
  (let* ((old-path (file-name book))
         (new-path (make-unique-name-in
                    (make-pathname :directory (pathname-directory old-path))
                    (format nil "~a.4k" (file-namestring old-path))))
         (new (load-notebook! (fork-at book index :file-name new-path)))
         (new-name (format nil "Fork of '~a'" (notebook-name book))))
    (rename-notebook! new new-name)
    (register-notebook! new)
    new))

;;;;;;;;;; Notebooks table and related functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *notebooks* (make-hash-table :test 'equal))

(defun loaded-books! ()
  (sort
   (loop for k being the hash-keys of *notebooks*
      for v being the hash-values of *notebooks*
      if (cl-fad:file-exists-p k)
      collect (list k v)
      else do (remhash k *notebooks*))
   #'string<= :key #'first))

(defmethod register-notebook! ((book notebook))
  (setf (gethash (notebook-id book) *notebooks*) book))

(defmethod get-notebook! ((name string))
  (let ((n (house::uri-decode name)))
    (aif (gethash n *notebooks*)
         it
         (let ((book (load-notebook! (pathname n))))
           (setf (gethash n *notebooks*) book)
           book))))
