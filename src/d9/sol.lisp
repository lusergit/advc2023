(ql:quickload :uiop)
(ql:quickload :str)

(defun solve-09-a ()
  (let* ((input-lines (uiop:read-file-lines #p"input.txt"))
        (histories (extract-histories input-lines)))
    (apply #'+ (mapcar #'predict histories))))

(defun solve-09-b ()
  (let* ((input-lines (uiop:read-file-lines #p"input.txt"))
        (histories (extract-histories input-lines)))
    (apply #'+ (mapcar #'extrapolate histories))))

(defun extract-histories (input-lines)
  (loop :for line :in input-lines
	:collect (mapcar #'parse-integer (str:split " " line))))

(defun predict (history)
  (let ((diffs (collect-differences history)))
    (reduce (lambda (a lb) (+ a (car (last lb)))) (rest diffs) :initial-value 0)))

(defun extrapolate (history)
  (let ((diffs (collect-differences history)))
    (reduce (lambda (a lb) (- (first lb) a)) (rest diffs) :initial-value 0)))

(defun collect-differences (sequence)
  (let ((current sequence)
        (result (list sequence)))
    (loop :for diff = (differences current)
          :do (setf current diff)
              (push diff result)
          :until (every #'zerop diff))
    result))

(defun differences (sequence)
    (loop :for (a b) :on sequence :while b
          :collect (- b a)))
