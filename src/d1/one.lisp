(defun get-file-lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun first-last-digit (string)
  (let* ((fst (find-if #'digit-char-p string))
	(snd (find-if #'digit-char-p (reverse string)))
	(numb (concatenate 'string (string fst) (string snd))))
    (parse-integer numb)))

(format t
	"Sum: ~s~%"
	(reduce '+ (mapcar 'first-last-digit  (get-file-lines "input.txt"))))
