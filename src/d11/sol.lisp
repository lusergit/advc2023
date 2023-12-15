(ql:quickload :uiop)

(defun getmap (lines)
  (let* ((chars (mapcar (lambda (line) (coerce line 'list)) lines))
	 (rows (length (car chars)))
	 (cols (length chars))
	 (themap (make-array (list rows cols) :initial-contents chars)))
    themap))

(defun find-positions (themap)
  (let ((positions '()))
    (dotimes (i (array-dimension themap 0))
      (dotimes (j (array-dimension themap 1))
        ; Add more nested loops for additional dimensions
        (when (eq #\# (aref themap i j))
          (push (list i j) positions))))
    positions))

(defun get-col (array column-index)
  (loop for i below (array-dimension array 0)
        collect (aref array i column-index)))

(defun get-row (array row-index)
  (loop for i below (array-dimension array 1)
        collect (aref array row-index i)))

(defun find-empty (themap &key (cols nil))
  (let* ((dim-bound (array-dimension themap (if cols 1 0)))
	 (empties '()))
    ;; (format t "Dim bound: ~s~%" dim-bound)
    (loop :for i :from 0 :below dim-bound
	  :do (if (reduce (lambda (acc x) (and acc (eq #\. x)))
			  (funcall (if cols 'get-col 'get-row) themap i)
			  :initial-value t)
		  (push i empties)))
    empties))

(defun included-blanks (start end blanks)
  (count-if (lambda (blank) (<= (min start end) blank (max start end))) blanks))

(defun galaxy-distance (a b blanks expansion-factor)
  (flet ((coord-distance (a b blanks)
           (+ (abs (- b a))
              (* (1- expansion-factor) (included-blanks a b blanks)))))
    (reduce #'+ (map 'list #'coord-distance a b blanks))))

(defun distances (galaxies blanks &key (expansion 2))
  (loop :for (g1 . tail) :on galaxies
	:collect (loop :for g2 :in tail
		       :collect (galaxy-distance g1 g2 blanks expansion))))

(defun solve1 ()
  (let* ((input-lines (uiop:read-file-lines #p"input.txt"))
	 (themap (getmap input-lines))
	 (empty-rows (find-empty themap :cols nil))
	 (empty-cols (find-empty themap :cols t))
	 (galaxies (find-positions themap))
	 (dists (distances galaxies (list empty-rows empty-cols))))
    (reduce #'+ (mapcar (lambda (set) (reduce #'+ set)) dists))))

(defun solve2 ()
  (let* ((input-lines (uiop:read-file-lines #p"input.txt"))
	 (themap (getmap input-lines))
	 (empty-rows (find-empty themap :cols nil))
	 (empty-cols (find-empty themap :cols t))
	 (galaxies (find-positions themap))
	 (dists (distances galaxies (list empty-rows empty-cols) :expansion 1000000)))
    (reduce #'+ (mapcar (lambda (set) (reduce #'+ set)) dists))))
