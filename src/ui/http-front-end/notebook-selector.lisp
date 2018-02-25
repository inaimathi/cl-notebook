(in-package :cl-notebook)

(define-handler (js/notebook-selector.js :content-type "application/javascript") ()
  (ps (defun notebook-link-template (notebook-path)
        (who-ps-html
         (:li (:a :href (+ "/#book=" notebook-path) notebook-path))))

      (defun selector-template (current-notebook)
        (who-ps-html
         (:div :class "notebook-selector"
          (:p :class "current-notebook" current-notebook)
          (:ul :class "loaded-books-list")
          (:input :class "filesystem-input")
          (:span :class "filesystem-view"))))

      (defun file-template (file)
        (who-ps-html
         (:li (:a :href (+ "/#book=" (@ file :string)) (@ file :name)))))

      (defun directory-template (directory)
        (who-ps-html
         (:li (:span :class "directory-link" (last (@ directory :directory))))))

      (defun filesystem-template (listing)
        (who-ps-html
         (:ul :class "filesystem-list"
              (+ (join (map directory-template (@ listing :directories)))
                 (join (map file-template (@ listing :files)))))))

      (defun filesystem! (input-elem elem directory)
        (get/json
         "/cl-notebook/system/ls" (create :dir directory)
         (lambda (dat)
           (dom-set elem (filesystem-template dat)))))

      (defun loaded-books! (elem)
        (get/json "/cl-notebook/loaded-books" (create)
                  (lambda (dat)
                    (dom-set
                     elem
                     (join (map notebook-link-template dat)) "Blah!"))))

      (defun notebook-selector! (selector)
        (let ((elem (by-selector selector)))
          (dom-set elem (selector-template (@ *notebook* id)))
          (loaded-books! (by-selector elem ".loaded-books-list"))
          (get/json "/cl-notebook/system/home-path" (create)
                    (lambda (initial-dir)
                      (setf (@ (by-selector elem ".filesystem-input") value) initial-dir)
                      (filesystem!
                       (by-selector elem ".filesystem-input") (by-selector elem ".filesystem-view")
                       initial-dir)))))))
