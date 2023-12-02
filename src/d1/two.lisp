(ql:quickload "cl-ppcre")

(defun get-file-lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun replace-in-string (string)
  (let* ((p1 (ppcre:create-scanner "one"))
	 (p2 (ppcre:create-scanner "two"))
	 (p3 (ppcre:create-scanner "three"))
	 (p4 (ppcre:create-scanner "four"))
	 (p5 (ppcre:create-scanner "five"))
	 (p6 (ppcre:create-scanner "six"))
	 (p7 (ppcre:create-scanner "seven"))
	 (p8 (ppcre:create-scanner "eight"))
	 (p9 (ppcre:create-scanner "nine"))
	 (s1 (ppcre:regex-replace p1 string "o1e"))
	 (s2 (ppcre:regex-replace p2 s1 "t2o"))
	 (s3 (ppcre:regex-replace p3 s2 "t3e"))
	 (s4 (ppcre:regex-replace p4 s3 "4"))
	 (s5 (ppcre:regex-replace p5 s4 "5e"))
	 (s6 (ppcre:regex-replace p6 s5 "6"))
	 (s7 (ppcre:regex-replace p7 s6 "7n"))
	 (s8 (ppcre:regex-replace p8 s7 "e8t"))
	 (s9 (ppcre:regex-replace p9 s8 "9e")))
    s9))

(defun first-last-digit (string)
  (let* ((string (replace-in-string string))
	 (fst (find-if #'digit-char-p string))
	 (snd (find-if #'digit-char-p (reverse string)))
	 (numb (concatenate 'string (string fst) (string snd))))
    (parse-integer numb)))

(format t
	"Sum: ~s~%"
	(reduce '+ (mapcar 'first-last-digit  (get-file-lines "input.txt"))))
