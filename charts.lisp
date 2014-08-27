(in-package #:cl-notebook)

(defun bar-graph (label/value-pairs &key x-axis y-axis title y-max)
  (destructuring-bind (max len) (loop for (k . v) in label/value-pairs 
                                      maximize v into m sum 1 into s
                                      finally (return (list m s)))
    (let* ((h (or y-max max))
           (bar-width (% (max .1 (float (/ 88 (max 1 len))))))
           (bar-margin (% (float (/ 10 (* 2 (max 1 len)))))))
      (with-html-output-to-string (*standard-output*)
        (:div :class "chart"
              (when title (htm (:div :class "title" (str title))))
              (:div :class "bar-graph"
                    (loop for (label . value) in label/value-pairs
                          for i from 0
                          for hider-height = (round (* 100 (/ (- h value) h)))
                          do (htm (:div :class "bar" :style (inline-css `(:width ,bar-width :margin ,bar-margin))
                                        (:div :class "hider" :style (inline-css `(:height ,(% hider-height))))
                                        (:div :class "spacer" :style (inline-css `(:height ,(% (- 100 hider-height)))))
                                        (:div :class "label" :style (inline-css `(,@(when (< i (* 3 (/ len 4))) '(:white-space nowrap))))
                                              (str label)))))))))))
