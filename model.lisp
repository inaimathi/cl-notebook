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
    (register-notebook! book)
    book))

(defmethod load-notebook! ((file-name pathname))
  (assert (cl-fad:file-exists-p file-name) nil "Nonexistent file ~s" file-name)
  (let* ((book (make-notebook :file-name file-name)))
    (with-open-file (s file-name :direction :input)
      (loop for entry = (fact-base::read-entry! s) while entry
	 do (incf (fact-base::entry-count book))
	 do (fact-base::apply-entry! book entry)))
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

(defmethod notebook-name ((book notebook))
  (caddar (lookup book :b :notebook-name)))

(defmethod rename-notebook! ((book notebook) (new-name string))
  "Takes a book and a new name.
Returns two values; the renamed book, and a boolean specifying whether the name was changed.
If the new name passed in is the same as the books' current name, we don't insert any new facts."
  (let* ((name-fact (first (lookup book :b :notebook-name)))
	 (same? (equal (third name-fact) new-name)))
    (unless same?
      (change! book name-fact (list (first name-fact) :notebook-name new-name)))
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

