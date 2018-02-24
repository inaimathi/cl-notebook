(in-package :cl-notebook)

(define-handler (js/notebook-selector.js :content-type "application/javascript") ()
  (ps (defun notebook-link-template (notebook-path)
        (who-ps-html
         (:li (:a :href (+ "/#book=" notebook-path) notebook-path))))

      (defun selector-template (current-notebook)
        (who-ps-html
         (:div
          (:h3 current-notebook)
          (:ul :class "loaded-books-list")
          (:span :class "filesystem-view"))))

      (defun filesystem! (elem)
        (dom-set elem "Blah blah blah!"))

      (defun loaded-books! (elem)
        (get/json "/cl-notebook/loaded-books" (create)
                  (lambda (dat)
                    (dom-set
                     elem
                     (join (map notebook-link-template dat)) "Blah!"))))

      (defun notebook-selector! (selector)
        (let ((elem (by-selector selector)))
          (console.log "Setting up in element" elem)
          (dom-set elem (selector-template (@ *notebook* id)))
          (loaded-books! (by-selector elem ".loaded-books-list"))
          (filesystem! (by-selector elem ".filesystem-view"))))))
