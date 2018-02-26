(in-package :cl-notebook)

(define-handler (js/notebook-selector.js :content-type "application/javascript") ()
  (ps
    (defvar *current-fs-listing* nil)

    (defun notebook-link-template (notebook current-notebook-id)
      (if (equal? current-notebook-id (@ notebook :path))
          (who-ps-html
           (:li :class "book-link current-book" :title (@ notebook :path) (@ notebook :title)))
          (who-ps-html
           (:li :class "book-link" :title (@ notebook :path)
                (:a :href (+ "/#book=" (@ notebook :path)) (@ notebook :title))))))

    (defvar filesystem-input-change
      (debounce
       (lambda (event elem)
         (console.log "BLAH" event elem)
         (let ((filtered (filter-fs-listing *current-fs-listing* (@ elem value))))
           (render-filesystem! (by-selector ".filesystem-view") filtered)))
       100))

    (defun filesystem-directory-click (directory)
      (setf (@ (by-selector ".filesystem-input") value) directory)
      (filesystem! (by-selector ".filesystem-view") directory))

    (defun selector-template (current-notebook)
      (who-ps-html
       (:div :class "notebook-selector"
             (:p :title (@ current-notebook :id) :class "current-notebook" (@ current-notebook :name))
             (:ul :class "loaded-books-list")
             (:input :class "filesystem-input" :onkeypress "filesystemInputChange(event, this)")
             (:span :class "filesystem-view"))))

    (defun file-template (file)
      (who-ps-html
       (:li :class "file-link"
            (:a :href (+ "/#book=" (@ file :string)) (@ file :name)))))

    (defun directory-template (directory)
      (let ((call (+ "filesystemDirectoryClick('" (@ directory :string) "')")))
        (who-ps-html
         (:li :class "directory-link"
              (:span :class "directory-link" :onclick call
                     (last (@ directory :directory)))))))

    (defun filesystem-template (listing)
      (who-ps-html
       (:ul :class "filesystem-list"
            (+ (join (map directory-template (or (@ listing :directories) (list))))
               (join (map file-template (or (@ listing :files) (list))))))))

    (defun filter-fs-listing (listing prefix)
      (let ((f (lambda (path) (chain (@ path :string) (starts-with prefix)))))
        (create :directories (filter f (@ listing :directories))
                :files (filter f (@ listing :files)))))

    (defun render-filesystem! (elem listing)
      (dom-set elem (filesystem-template listing)))

    (defun filesystem! (elem directory)
      (get/json "/cl-notebook/system/ls" (create :dir directory)
                (lambda (dat)
                  (console.log "GOT ls RESPONSE!" directory dat)
                  (setf *current-fs-listing* dat)
                  (render-filesystem! elem dat))))

    (defun loaded-books! (elem current-notebook-id)
      (get/json "/cl-notebook/loaded-books" (create)
                (lambda (dat)
                  (dom-set
                   elem
                   (join
                    (map
                     (lambda (bk)
                       (notebook-link-template bk current-notebook-id))
                     dat))))))

    (defun notebook-selector! (selector)
      (let ((elem (by-selector selector)))
        (dom-set elem (selector-template *notebook*))
        (loaded-books! (by-selector elem ".loaded-books-list") (@ *notebook* id))
        (get/json "/cl-notebook/system/home-path" (create)
                  (lambda (initial-dir)
                    (setf (@ (by-selector elem ".filesystem-input") value) initial-dir)
                    (filesystem!
                     (by-selector elem ".filesystem-view")
                     initial-dir)))))))
