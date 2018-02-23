(in-package :cl-notebook)

(define-handler (js/notebook-selector-widget.js :content-type "application/javascript") ()
  (ps (console.log "Hello from notebook-selector-widget")))
