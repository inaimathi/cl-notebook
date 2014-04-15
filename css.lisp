(in-package #:cl-notebook)

(defparameter +css-input+
  `(:border "2px solid #ccc" :border-right-color "#aaa" :border-bottom-color "#aaa" :border-radius 4px :height 24px :font-weight bold))

(define-closing-handler (css/notebook.css :content-type "text/css") ()
  (cl-css:css
   `((body :font-family sans-serif)
     
     ("button" ,@+css-input+ :min-width 34px :font-size large :float left :margin-right 5px :color "#666")
     ("button.right" :float right)
     ("button.genericon" :font-size x-large)
     ("button .btn-text" :font-size medium :display inline-block)
     ("button:hover" :color "#000")

     (select ,@+css-input+ :color "#666" :margin-right 5px)
     ("select:hover" :color "#000" :background-color "#eee")

     (input.text ,@+css-input+ :padding 3px)

     (.book-title :margin "20px 10px 10px 5px" :padding 0px)
     (".book-title h1" :cursor pointer :margin 0px :padding 0px)

     (.main-controls 
      :background-color "#eee" :border "2px solid #ccc" :border-radius "0px 0px 5px 5px"
      :padding 8px
      :z-index 10 :position fixed :top -40px
      :width 60% :left 50% :margin-left -30%)
     (".main-controls:hover" :top -2px)
     (".main-controls button" :height 32px)
     (".main-controls select" :height 32px :font-size large :width 256px)
     
     (.cells :list-style-type none :padding 0px :margin 0px)
     (".cells .cell" :padding 5px :margin-bottom 10px :border-top "3px solid transparent" :background-color "#fff")
     (".cells .cell.code" :background-color "#eee")

     (".cell .controls"
      :display none :position absolute :margin-top -41px :padding 5px
      :background-color "#eee" :border "2px solid #ccc" :border-bottom none :border-radius "5px 5px 0px 0px")
     (".cell .controls button" :width 32px)
     (".cell .controls span"
      :height 19px :width 31px :font-size x-large :float left :margin-right 5px :color "#666"
      :padding-top 5px :padding-left 3px :cursor move)
     (".cell .controls span:hover" :color "#000")

     (".cell blockquote" :font-style oblique :border-left "2px solid #eee" :padding 10px)

     (".cell:hover" :border-top "3px solid #ccc" :z-index 15)
     (".cell:hover .controls" :display block)

     (.result :border "1px solid #ccc" :background-color "#fff" :list-style-type none :margin 0px :margin-top 5px :padding 0px)
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
     (".condition-contents .condition-property .label" :display inline-block :margin-right 5px :font-size small))))
