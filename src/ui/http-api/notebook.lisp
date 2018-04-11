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
