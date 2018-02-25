(in-package :cl-notebook)

(define-handler (js/notebook-selector.js :content-type "application/javascript") ()
  (ps (defun notebook-link-template (notebook current-notebook-id)
        (if (equal? current-notebook-id (@ notebook :path))
            (who-ps-html
             (:li :class "book-link current-book" :title (@ notebook :path) (@ notebook :title)))
            (who-ps-html
             (:li :class "book-link" :title (@ notebook :path)
                  (:a :href (+ "/#book=" (@ notebook :path)) (@ notebook :title))))))

      (defun filesystem-input-change (elem)
        (console.log "VALUE CHANGED" elem (@ elem value)))

      (defun filesystem-directory-click (directory)
        (console.log "GOT A CLICK ON '" directory "'")
        (setf (@ (by-selector ".filesystem-input") value) directory))

      (defun selector-template (current-notebook)
        (who-ps-html
         (:div :class "notebook-selector"
          (:p :title (@ current-notebook :id) :class "current-notebook" (@ current-notebook :name))
          (:ul :class "loaded-books-list")
          (:input :class "filesystem-input" :onkeydown "filesystemInputChange(this)")
          (:span :class "filesystem-view"))))

      (defun file-template (file)
        (who-ps-html
         (:li :class "file-link"
              (:a :href (+ "/#book=" (@ file :string)) (@ file :name)))))

      (defun directory-template (directory)
        (let ((call (+ "filesystemDirectoryClick('" (@ directory :string) "')")))
          (console.log "OUTPUTTING" directory)
          (console.log "   -- " call)
          (who-ps-html
           (:li :class "directory-link"
                (:span :class "directory-link" :onclick call
                       (last (@ directory :directory)))))))

      (defun filesystem-template (listing)
        (who-ps-html
         (:ul :class "filesystem-list"
              (+ (join (map directory-template (@ listing :directories)))
                 (join (map file-template (@ listing :files)))))))

      (defun filter-fs-listing (listing prefix)
        (let ((f (lambda (path) (chain (@ path :string) (starts-with prefix)))))
          (create :directories (filter f (@ listing :directories))
                  :files (filter f (@ listing :files)))))

      (defun filesystem! (input-elem elem directory)
        (get/json
         "/cl-notebook/system/ls" (create :dir directory)
         (lambda (dat)
           (console.log "FILTERED" (filter-fs-listing dat (+ directory ".b")))
           (dom-set elem (filesystem-template dat)))))

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
                       (by-selector elem ".filesystem-input") (by-selector elem ".filesystem-view")
                       initial-dir)))))))
