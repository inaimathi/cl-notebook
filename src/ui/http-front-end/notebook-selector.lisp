(in-package :cl-notebook)

(define-handler (js/notebook-selector.js :content-type "application/javascript") ()
  (ps
    (defvar *current-fs-listing* nil)

    (defun common-prefix-of (starting-point strings)
      (let ((max-prefix (loop for s in strings minimize (length s)))
            (same-at? (lambda (ix)
                        (let ((chr (aref strings 0 ix))
                              (res t))
                          (loop for s in strings unless (string= (aref s ix) chr) do (setf res f))
                          res))))
        (+ starting-point
           (join
            (loop for i from (length starting-point) to max-prefix
               when (same-at? i) collect (aref strings 0 i))))))

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
         (let* ((val (@ elem value))
                (dval (if (chain val (ends-with "/")) val (+ val "/")))
                (filtered (filter-fs-listing *current-fs-listing* val))
                (fnames (map (lambda (d) (@ d :string)) (@ filtered :files)))
                (dirnames (map (lambda (d) (@ d :string)) (@ filtered :directories)))
                (key-code (@ event key-code)))
           (cond
             ((= 9 key-code)
              (chain event (prevent-default))
              (chain elem (focus))
              (let ((completed (common-prefix-of val (chain fnames (concat dirnames)))))
                (setf (@ elem value) completed)
                (when (chain completed (ends-with "/"))
                  (filesystem! (by-selector ".filesystem-view") completed))))
             ((and (= key-code 13) (member? val fnames))
              (setf (@ window location href) (+ "/#book=" val)))
             ((and (member? key-code (list 13 47)) (member? dval dirnames))
              (filesystem! (by-selector ".filesystem-view") dval))
             ((and (= 13 key-code))
              (new-book val))
             ((or (= 8 key-code) (= 46 key-code))
              (filesystem!
               (by-selector ".filesystem-view")
               (+ (chain val (split "/") (slice 0 -1) (join "/")) "/")
               :filter? f))
             ((not (= 0 (@ event char-code)))
              (unless (and (= 0 (length (@ filtered :directories))) (= 0 (length (@ filtered :files))))
                (render-filesystem! (by-selector ".filesystem-view") filtered))))))
       200))

    (defun filesystem-directory-click (directory)
      (setf (@ (by-selector ".filesystem-input") value) directory)
      (filesystem! (by-selector ".filesystem-view") directory))

    (defun selector-template ()
      (who-ps-html
       (:div :class "notebook-selector"
             (:ul :class "loaded-books-list")
             (:input :class "filesystem-input" :onkeydown "filesystemInputChange(event, this)")
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
            (+ (join (map directory-template (@ listing :directories)))
               (join (map file-template (@ listing :files)))))))

    (defun filter-fs-listing (listing prefix)
      (if listing
          (let ((f (lambda (path) (chain (@ path :string) (starts-with prefix)))))
            (create :directories (filter f (@ listing :directories))
                    :files (filter f (@ listing :files))))
          (create :directories (list) :files (list))))

    (defun render-filesystem! (elem listing)
      (dom-set elem (filesystem-template listing)))

    (defun filesystem! (elem directory &key (filter? t))
      (get/json "/cl-notebook/system/ls" (create :dir directory)
                (lambda (dat)
                  (setf *current-fs-listing*
                        (create :directories (or (@ dat :directories) (list))
                                :files (or (@ dat :files) (list))))
                  (render-filesystem!
                   elem
                   (if filter?
                       (filter-fs-listing
                        *current-fs-listing*
                        (@ (by-selector ".filesystem-input") value))
                       *current-fs-listing*)))))

    (defun get-loaded-books! (elem current-notebook-id)
      (get/json
       "/cl-notebook/loaded-books" (create)
       (lambda (dat)
         (console.log "GOT LOADED BOOKS!" dat)
         (dom-set
          elem
          (join
           (map
            (lambda (bk)
              (notebook-link-template bk current-notebook-id))
            dat))))))

    (defun notebook-selector! (selector)
      (let ((elem (by-selector selector)))
        (dom-set elem (selector-template))
        (get-loaded-books!
         (by-selector elem ".loaded-books-list")
         (@ *notebook* id))
        (chain (by-selector elem ".filesystem-input") (focus))
        (get/json "/cl-notebook/system/home-path" (create)
                  (lambda (initial-dir)
                    (setf (@ (by-selector elem ".filesystem-input") value) initial-dir)
                    (filesystem! (by-selector elem ".filesystem-view") initial-dir)))))))
