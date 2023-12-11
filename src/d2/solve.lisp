(ql:quickload :split-sequence)

(defmacro ssq (char sequence)
  `(split-sequence:split-sequence ,char ,sequence))

(defvar *qualify* '((red . 12)
  		    (green . 13)
  		    (blue . 14)))

(defun fetch-at-least (color visions)
  (cons color
    	(apply #'max
    	       (mapcar #'cdr
    		       (remove-if-not #'(lambda (vision)
    					  (eq (car vision) color))
    				      visions)))))

(defun parse-pulls (pulls)
  (let* ((visions (mapcar #'(lambda (vision)
    			      (let* ((clean (string-trim " " vision))
    				     (val (read-from-string clean))
    				     (color (read-from-string (cadr (ssq #\Space clean)))))
    				(cons color val)))
    			  (ssq #\, pulls))))
    (mapcar #'(lambda (color)
    		(fetch-at-least color visions))
    	    '(red green blue))))

(defun valid-game-p (at-leasts)
  (reduce #'(lambda (a b)
    	      (and a b))
    	  (mapcar #'(lambda (color-at-least)
    		      (>= (cdr (assoc (car color-at-least) *qualify*))
    			  (cdr color-at-least)))
    		  at-leasts)
    	  :initial-value t))

(defun parse-line-part-1 (line)
  (let* ((base-split (ssq #\: line))
    	 (id (parse-integer (cadr (ssq #\Space (car base-split)))))
    	 (at-leasts (parse-pulls (substitute #\, #\; (cadr base-split))))
    	 (valid (valid-game-p at-leasts)))
    (if (valid-game-p at-leasts) id 0)))

(defun parse-line-part-2 (line)
  (let* ((base-split (ssq #\: line))
    	 (id (parse-integer (cadr (ssq #\Space (car base-split)))))
    	 (at-leasts (parse-pulls (substitute #\, #\; (cadr base-split)))))
    (apply #'* (mapcar #'cdr at-leasts))))

(defun solve-1 (filename)
  (with-open-file (stream filename)
    (do ((acc 0 (+ acc (parse-line-part-1 (read-line stream)))))
    	((not (listen stream)) acc))))

(defun solve-2 (filename)
  (with-open-file (stream filename)
    (do ((acc 0 (+ acc (parse-line-part-2 (read-line stream)))))
    	((not (listen stream)) acc))))

(let ((fname "input.txt"))
  (print (solve-1 fname))
  (print (solve-2 fname)))
