(ql:quickload :split-sequence)
(ql:quickload :str)

(defun parse-line (line)
  (let* ((ints (second (split-sequence:split-sequence #\: line))))
    (str:words ints)))

(defun parse-line-2 (line)
  (let* ((ints (second (split-sequence:split-sequence #\: line))))
    (format nil "~{~a~}" (str:words ints))))

(defun todist (press time)
  (* press (- time press)))

(defun findmax (game)
  (let* ((time (parse-integer (first game)))
	 (distance (parse-integer (second game)))
	 (winnable 0))
    (loop for i from 0 to time do
      (let* ((dist (todist i time)))
	(if (> dist distance) (setf winnable (+ 1 winnable)))))
    winnable))

(defun solve1 ()
  (with-open-file (stream "input.txt")
    (let* ((times (parse-line (read-line stream)))
	   (distances (parse-line (read-line stream)))
	   (td-list (mapcar #'list times distances)))
      (reduce #'* (mapcar 'findmax td-list)))))

(defun solve2 ()
  (with-open-file (stream "input.txt")
    (let* ((times (parse-line-2 (read-line stream)))
	   (distances (parse-line-2 (read-line stream)))
	   (td-list (list times distances)))
      (findmax td-list))))
