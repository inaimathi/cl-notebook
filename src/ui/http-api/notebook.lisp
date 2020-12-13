(in-package #:cl-notebook)

(define-json-handler (cl-notebook/notebook/export) ((book :notebook) (format :export-format))
  (hash
   :contents (export-as format book)
   :mimetype (mimetype-of format)
   :name (filename-of format book)))

(define-json-handler (cl-notebook/notebook/fork-at) ((book :notebook) (index :integer))
  (let ((new (fork-at! book index)))
    (publish-update! new 'new-book :book-name (notebook-name new))
    (hash :facts (current new) :history-size (total-entries new) :id (notebook-id new) :book-name (notebook-name new))))

(define-json-handler (cl-notebook/notebook/reorder-cells) ((book :notebook) (cell-order :json))
  (reorder-cells! book cell-order)
  (publish-update! book 'reorder-cells :new-order cell-order)
  :ok)

(define-json-handler (cl-notebook/notebook/rewind) ((book :notebook) (index :integer))
  (hash :facts (rewind-to book index) :history-size (total-entries book) :history-position index :id (notebook-id book)))

(define-json-handler (cl-notebook/notebook/current) ((book :notebook))
  (hash :facts (current book) :history-size (total-entries book) :id (notebook-id book)))

(define-json-handler (cl-notebook/notebook/new) ((path :nonexistent-file))
  (let* ((book (new-notebook! path))
	 (cell-id (new-cell! book :cell-type :code :cell-language :common-lisp))
	 (cont-fact (first (lookup book :a cell-id :b :contents))))
    (change! book cont-fact (list cell-id :contents ";;; TODO - awesome things"))
    (write! book)
    (publish-update! book 'new-book :book-name (notebook-name book))
    (hash :facts (current book) :history-size (total-entries book) :id (notebook-id book))))

(define-json-handler (cl-notebook/notebook/load) ((path :existing-filepath))
  (let ((book (load-notebook! path)))
    (publish-update! book 'new-book :book-name (notebook-name book))
    (hash :facts (current book) :history-size (total-entries book) :id (notebook-id book))))

(define-json-handler (cl-notebook/notebook/repackage) ((book :notebook) (new-package :string))
  (eval-package book new-package)
  :ok)

(define-json-handler (cl-notebook/notebook/rename) ((book :notebook) (new-name :string))
  (multiple-value-bind (book renamed?) (rename-notebook! book new-name)
    (when renamed?
      (unless (or (lookup book :b :package-edited?) (lookup book :b :package-error))
	(repackage-notebook! book (default-package book))
	(publish-update! book 'finished-package-eval :contents (notebook-package-spec-string book)))
      (publish-update! book 'rename-book :new-name new-name)))
  :ok)
