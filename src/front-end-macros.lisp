(in-package #:cl-notebook)

(defparameter +mod-keys+
  `((shift? (@ event shift-key)) (alt? (@ event alt-key)) (ctrl? (@ event ctrl-key)) (meta? (@ event meta-key))))

(defparameter +key-codes+
  `((<ret> 13) (<esc> 27) (<space> 32) (<up> 38) (<down> 40) (<left> 37) (<right> 39)))

(defpsmacro key-listener (&body key/body-pairs)
  `(lambda (event)
     (let (,@+mod-keys+ 
	   ,@+key-codes+
	   (key-code (or (@ event key-code) (@ event which))))
       (cond 
	 ,@(loop for (key body) on key/body-pairs by #'cddr
	      collect `((= key-code ,(if (stringp key) `(chain ,key (char-code-at 0)) key)) 
			,body))))))

(defpsmacro $aif (test if-true &optional if-false)
  `(let ((it ,test))
     (if it ,if-true ,if-false)))
