(in-package #:cl-notebook)

(defparameter +css-input+
  `(:border "2px solid #ccc" :border-right-color "#aaa" :border-bottom-color "#aaa" :border-radius 4px :height 24px :font-weight bold))

(define-handler (css/notebook.css :content-type "text/css") ()
  (cl-css:css
   `((body :font-family sans-serif :overflow-x hidden)

     ("button" ,@+css-input+ :min-width 34px :font-size 1em :float left :margin-right 1% :color "#666" :cursor pointer)
     ("button.right" :float right)
     ("button.genericon" :font-size 1.4em)
     ("button .btn-text" :display inline-block)
     ("button:hover" :color "#000")

     (select ,@+css-input+ :color "#666" :margin-right 1%)
     ("select:hover" :color "#000" :background-color "#eee")

     (input.text ,@+css-input+ :padding 3px)

     (.book-title :margin "10px 10px 5px 5px" :padding 0px)
     (".book-title h1" :cursor pointer :margin 0px :margin-bottom 5px :padding 0px)
     (".book-title input" :margin-bottom 5px :width 50%)
     (.book-package :margin "0px 5px 10px 5px" :padding 0px)

     (.main-controls
      :background-color "#eee" :border "2px solid #ccc" :border-radius "0px 0px 5px 5px"
      :z-index 10 :position fixed :bottom 0px :padding 0px :margin 0px
      :width 100% :padding 1%)
     (".main-controls #book-history-slider" :width 98% :margin "5px 0px")
     (".main-controls button" :width 20% :min-height 32px)
     (".main-controls select" :font-size 1em :width 24% :padding-top 4px :min-height 32px)
     (".main-controls .ui-element" :font-size 1em :width 28% :padding-top 4px :min-height 32px)
     (".main-controls input" :margin 0px :padding 0px :margin-bottom 3px)

     (.notebook-selector :background-color "#eee" :padding 10px :max-height 80vh)
     (".notebook-selector .filesystem-input" :width 80%)
     (".notebook-selector .loaded-books-list" :list-style-type none :columns "5 100px" :background-color white :padding-left 5px :height 20% :overflow auto)
     (".notebook-selector .loaded-books-list li" :padding 5px)
     (".notebook-selector .filesystem-list" :list-style-type none :columns "5 100px" :background-color white :padding-left 5px :margin 0px :height 50% :overflow auto)

     (.thread-controls :padding "8px 0px" :border-radius "5px"
                       :font-weight bold :color "rgba(255, 255, 255, .6)"
                       :width 98% :background-color "rgba(50, 50, 200, .4)" :border "2px solid rgba(50, 50, 200, .6)"
                       :margin-top 10px)
     (".thread-controls .notice" :padding-left 5px)
     (".thread-controls img" :height 1.6em :opacity .6)
     (".thread-controls button" :background-color "rgba(50, 50, 200, .4)" :font-size 1em
                                :color "rgba(255, 255, 255, .6)" :border-color "rgba(255, 255, 255, .6)"
                                :margin-right 8px)
     (".thread-controls button:hover" :border-color white :color white :background-color "rgba(75, 75, 200, .4)")

     (.notebook-arg-hint :position absolute :z-index 8 :padding 3px :border "1px solid #ccc" :border-radius 3px :background-color white :font-size small)
     (".notebook-arg-hint span" :margin-right 6px)
     (".notebook-arg-hint span:last-child" :margin-right 0px)
     (".notebook-arg-hint .name")
     (".notebook-arg-hint .modifier" :font-style oblique)

     (.cells :list-style-type none :padding 0px :margin 0px :margin-bottom 20%)
     (".cells .cell" :padding 5px :margin-bottom 10px :border-top "3px solid transparent" :background-color "#fff")
     (".cells .cell.stale" :border "2px solid orange")
     (".cells .cell.code" :background-color "#eee")

     (".cell .controls"
      :display none :position absolute :margin-top -41px :padding 5px :padding-right 10px
      :background-color "#eee" :border "2px solid #ccc" :border-bottom none :border-radius "5px 5px 0px 0px" :z-index 8 :white-space nowrap)
     (".cell .controls button" :width 32px)
     (".cell .controls span"
      :height 19px :width 31px :font-size 1.4em :float left :margin-right 1% :color "#666"
      :padding-top 5px :padding-left 3px :cursor move)
     (".cell .controls span:hover" :color "#000")

     (".cell blockquote" :font-style oblique :border-left "2px solid #eee" :padding 10px)

     (".cell:hover" :border-top "3px solid #ccc" :z-index 15)
     (".cell:hover .controls" :display block)

     (.result :border "1px solid #ccc" :background-color "#fff" :list-style-type none :margin 0px :margin-top 5px :padding 0px :white-space pre-wrap)
     (.stdout :margin 0px :padding 5px :color "#8b2252" :background-color "#efefef")
     (".result li" :padding 5px)
     (".result .type" :color "#228b22")

     (".warnings .condition-contents"
      :background-color "#fc6" :color "#c60" :border "1px solid #c60"
      :padding 5px :margin "5px 0px")

     (".result .error" :background-color "#fdd" :color "#933")
     (.condition-contents :list-style-type none :margin 0px :padding 0px)
     (".condition-contents .condition-type" :font-weight bolder)
     (".condition-contents .condition-property" :font-style oblique)
     (".condition-contents .condition-property .label" :display inline-block :margin-right 5px :font-size .8em)

     ("#macro-expansion"
      :width 60% :height 95% :position :fixed :top 3% :right 0 :opacity 0.6
      :z-index 9 :border-radius 5px :border "2px solid #ccc" :background-color "#eee")
     ("#macro-expansion .CodeMirror" :height 100% :width 100%))))

(defparameter *addon-css-rules* (make-hash-table :test 'equalp))

(defun define-css (name rules)
  (setf (gethash name *addon-css-rules*) rules)
  (publish-update! nil 'addon-updated :addon-type :css :addon-name name)
  (cl-css:css rules))

(define-handler (css/notebook-addons.css :content-type "text/css") ()
  (cl-css:css (loop for v being the hash-values of *addon-css-rules* append v)))
