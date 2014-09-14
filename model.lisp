(in-package :cl-notebook)

;;;;;;;;;; Notebook and constructor/destructor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass notebook (fact-base)
  ((namespace :accessor namespace :initform (find-package :cl-notebook) :initarg :namespace)))

(defun make-notebook (&key (file-name (merge-pathnames (fact-base::temp-file-name) *books*)))
  (make-instance 
   'notebook :file-name file-name :in-memory? t
   :index (fact-base::make-index *default-indices*) 
   :history (fact-base::queue)))

(defun new-notebook! (name)
  (let ((book (make-notebook)))
    (insert-new! book :notebook-name name)
    (insert-new! 
     book :notebook-package (default-package book))
    (setf (namespace book) (notebook-package! book))
    (register-notebook! book)
    book))

(defmethod load-notebook! ((file-name pathname))
  (assert (cl-fad:file-exists-p file-name) nil "Nonexistent file ~s" file-name)
  (let* ((book (make-notebook :file-name file-name)))
    (with-open-file (s file-name :direction :input)
      (loop for entry = (fact-base::read-entry! s) while entry
	 do (incf (fact-base::entry-count book))
	 do (fact-base::apply-entry! book entry)))
    (setf (namespace book) (notebook-package! book))
    (eval-notebook book)
    (register-notebook! book)))

(defmethod kill! ((book notebook))
  (let ((trash-name (make-unique-name-in *trash*  (file-namestring (file-name book)))))
    (rename-file (file-name book) trash-name)
    (remove-notebook! book)))

;;;;;;;;;; Notebook methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod new-cell! ((book notebook) &key (cell-language :common-lisp) (cell-type :code))
  (multi-insert! book `((:cell nil) (:cell-type ,cell-type) (:cell-language ,cell-language) (:contents "") (:result ""))))

(defmethod notebook-id ((book notebook))
  (file-namestring (file-name book)))

(defmethod default-package ((book notebook))
  (format nil "(defpackage ~s~%  (:use :cl :fact-base :cl-notebook))" (notebook-name book)))

(defmethod notebook-name ((book notebook))
  (caddar (lookup book :b :notebook-name)))

(defmethod notebook-cell-order ((book notebook))
  (let ((all-ids (reverse (caddar (lookup book :b :cell-order)))))
    (loop for (id b c) in (lookup book :b :cell :c nil) do (pushnew id all-ids))
    (reverse all-ids)))

(defmethod notebook-package-spec-string ((book notebook))
  (caddar (lookup book :b :notebook-package)))

(defmethod notebook-package-spec ((book notebook))
  (let ((default (default-package book)))
    (handler-case
	(or (read-from-string (notebook-package-spec-string book)) (read-from-string default))
      (error ()
	(read-from-string default)))))

(defun load-dependencies (package-term)
  ;; look through the term. Load all packages that appear in
  ;; (:use . <here>)
  ;; (:import-from <here> . symbols)
  ;; (:shadowing-import-from <or here> . symbols)
  )

(defmethod notebook-package! ((book notebook))
  (let ((spec (notebook-package-spec book)))
    (or (find-package (second spec)) (eval spec))))

(defmethod repackage-notebook! ((book notebook) (new-package string))
  (let ((package-form (read-from-string new-package))
	(package-fact (first (lookup book :b :notebook-package)))
	(old-name (package-name (namespace book))))
    (if (string= new-package (third package-fact))
	(values book nil)
	(handler-case
	    (progn 
	      (setf (namespace book) (rename-package (namespace book) (second package-form)))
	      (handler-bind (#+sbcl (sb-ext:name-conflict 
				     (lambda (e)
				       (declare (ignore e))
				       (invoke-restart 'sb-impl::take-new))))
		(eval package-form))
	      (if package-fact ;; TODO - remove conditional eventually. All notebooks should have such facts.
		  (change! book package-fact (list (first package-fact) :notebook-package new-package))
		  (insert-new! book :notebook-package new-package))
	      (values book t))
	  (error ()
	    (setf (namespace book) (rename-package (namespace book) old-name))
	    (values book nil))))))

(defmethod rename-notebook! ((book notebook) (new-name string))
  "Takes a book and a new name.
Returns two values; the renamed book, and a boolean specifying whether the name was changed.
If the new name passed in is the same as the books' current name, we don't insert any new facts."
  (let* ((name-fact (first (lookup book :b :notebook-name)))
	 (same? (equal (third name-fact) new-name)))
    (unless same?
      (change! book name-fact (list (first name-fact) :notebook-name new-name))
      (unless (lookup book :b :package-edited?)
	(repackage-notebook! book (default-package book))))
    (values book (not same?))))

;;;;;;;;;; Notebooks table and related functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *notebooks* (make-hash-table :test 'equal))

(defun ordered-books ()
  (sort
   (loop for k being the hash-keys of *notebooks*
      for v being the hash-values of *notebooks*
      collect (list k (notebook-name v)))
   #'string<= :key #'second))

(defmethod remove-notebook! ((book notebook))
  (remhash (notebook-id book) *notebooks*))

(defmethod register-notebook! ((book notebook))
  (setf (gethash (notebook-id book) *notebooks*) book))

(defmethod get-notebook ((name string))
  (gethash name *notebooks*))

