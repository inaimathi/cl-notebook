(in-package :cl-notebook)

(define-handler (js/notebook-selector.js :content-type "application/javascript") ()
  (ps (console.log "Hello from notebook-selector-widget")

      (defun notebook-link-template (notebook-path)
        (who-ps-html
         (:li (:a :href (+ "/#book=" notebook-path) notebook-path))))

      (defun selector-template (current-notebook loaded-notebooks filesystem-start)
        (who-ps-html
         (:div
          (:h3 current-notebook)
          (:ul :class "loaded-books-list"
               (join (map #'notebook-link-template loaded-notebooks))))))

      (defun notebook-selector! (selector)
        (console.log "Setting up in element" (by-selector selector))
        (dom-set (by-selector selector)
                 (selector-template
                  (@ *notebook* id)
                  (list "/a/book.book"
                        "/b/book.book"
                        "/b/another-book.book"
                        "/c/yet-another-book.book"))))))
