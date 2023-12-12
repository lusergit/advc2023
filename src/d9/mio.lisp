(ql:quickload :split-sequence)

;; macro
(defmacro ssq (char seq)
  `(split-sequence:split-sequence ,char ,seq))

;; test if list is composed of zeroes
(defun is-all-zero (list)
  (reduce
   (lambda (x y) (and (eq 0 y) x)) list :initial-value t))

(defun to-numbers (line)
  (let* ((to-strings (ssq #\Space line)))
    (map 'list #'parse-integer to-strings)))

(defun dif-list (list)
  (loop :for (a b) :on list :by #'cdr :while b 
        :collect (- b a)))

;; line into first element of the triangle
(defun parse-line (line)
  (let* ((seq0 (to-numbers line))
	 (next (dif-list seq0)))
    (cons seq0
	  (loop while (not (is-all-zero next))
		collect next
		do (setf next (dif-list next))))))

(defun next-number (seqs)		;given a list of sequences
					;return next number in
					;sequence
  (let* ((lasts
	   (mapcar (lambda (x) (car (last x))) seqs)))
    (reduce #'+ lasts)))

(defun back-number (seqs)		;given a list of sequences
					;return next number in
					;sequence
  (let* ((firsts
	   (reverse (mapcar #'first seqs))))
    (reduce #'- (print  firsts))))

(defun solve1 ()
  (with-open-file (stream "input.txt")
    (reduce #'+ (loop for line = (read-line stream nil nil)
		      while line
		      collect (next-number (parse-line line))))))

(defun solve2 ()
  (with-open-file (stream "input.txt")
    (reduce #'+ (loop for line = (read-line stream nil nil)
		      while line
		      collect (back-number (parse-line line))))))
