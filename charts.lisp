(in-package #:cl-notebook)

(defun draw-bar-graph (label/value-pairs &key x-axis y-axis title y-max)
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
					   (fmt "~a (~a)" label value)))))))))))

(defun draw-svg-bar-graph (label/value-pairs &key x-axis y-axis title y-max)
  (destructuring-bind (max len) (loop for (k . v) in label/value-pairs 
				   maximize v into m sum 1 into s
				   finally (return (list m s)))
    (let* ((h (or y-max max))
           (bar-width (max .1 (float (/ 88 (max 1 len)))))
           (bar-margin (float (/ 10 (* 2 (max 1 len))))))
      (with-html-output-to-string (*standard-output*)
        (:svg :class "chart bar-graph"
              (when title (htm (:text :class "title" :x "1%" :y "10%" (str title))))
              (loop for (label . value) in label/value-pairs
		 for i from 0
		 do (let ((x (% (+ (* bar-width i) (* i bar-margin)))))
		      (htm (:g :class "bar"
			       (:rect :x x :y (% (round (* 100 (/ (- h value) h)))) :width (% bar-width) :height (% (round (* 100 (/ value h)))))
			       (:text :class "label" :x x :y (% (+ 5 h)) (fmt "~a (~a)" label value)))))))))))

(defmacro bar-graph (label/value-pairs &key x-axis y-axis title y-max)
  `(str (draw-bar-graph ,label/value-pairs :x-axis ,x-axis :y-axis ,y-axis :title ,title :y-max ,y-max)))
