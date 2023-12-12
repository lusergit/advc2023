(ql:quickload :split-sequence)

(defmacro ssq (char sequence)
  `(split-sequence:split-sequence ,char ,sequence))

;; Char to int rapresenting value
(defun char-to-card (char)
  (cond ((eq char #\A) 14)
	((eq char #\K) 13)
	((eq char #\Q) 12)
	((eq char #\J) 11)
	((eq char #\T) 10)
	(t (parse-integer (string char)))))

;; Card vec
(defvar *cards* (loop for i from 2 to 14 collect i))

;; list of cards to vector rapresenting multiplicity, removing non
;; present elements
(defun list-to-vec (cards)
  (let* ((occurrences (loop for el in *cards* collect (count el cards))))
    (sort 
     (mapcar 'list *cards* occurrences)
	  #'(lambda (x y)
	      (cond ((> (second x) (second y)) t)
		    ((and (eq (second x) (second y))
			  (> (first x) (first y))) y)
		    (t nil))))))

(defun vec-to-type (hand)
  (cond ((eq 5 (second (first hand))) 6)
	((eq 4 (second (first hand))) 5)
	((and (eq 3 (second (first hand)))
	      (eq 2 (second (second hand)))) 4)
	((eq 3 (second (first hand))) 3)
	((and (eq 2 (second (first hand)))
	      (eq 2 (second (second hand)))) 2)
	((eq 2 (second (first hand))) 1)
	(t 0)))

(defun rec-compair (h1 h2)
  (reduce (lambda (x y) (and x y))
	  (mapcar #'>= h1 h2)))

(defun str-to-list (str)
  (loop for char across str collect char))

(defun parse-line (line)
  (let* ((hand-bid (ssq #\Space line))
	 (hand (first hand-bid))
	 (bid (second hand-bid)))
    (list (mapcar #'char-to-card (str-to-list hand))
	  (parse-integer bid))))

(defun sort-hands (h1 h2)
  (let* ((t1 (vec-to-type (list-to-vec (first h1))))
	 (t2 (vec-to-type (list-to-vec (first h2)))))
    (cond ((< t1 t2) t)
	  ((> t1 t2) nil)
	  (t (let* ((s1 (sort (first h1) #'<))
		    (s2 (sort (first h2) #'<)))
	       (rec-compair s1 s2))))))

(defun solve1 ()
  (with-open-file (stream "input.txt")
    (let* ((sorted
	   (sort (loop for line = (read-line stream nil nil)
		       while line
		       collect (parse-line line))
		 'sort-hands)))
      ;; (print sorted)
      ;; very dirty but still should work
      (reduce #'+ (loop for i from 1 to (length sorted)
			collect (* i (second (nth (- i 1) sorted))))))))
