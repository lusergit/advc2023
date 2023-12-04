(ql:quickload :cl-ppcre)

(defun find-numbers (line)
  (loop :for (a b) :on (ppcre:all-matches "(\\d+)" line) :by #'cddr
        :collect (list (parse-integer (subseq line a b)) a b)))

(defun symbol-char-p (chr)
  (not (or (null chr) (digit-char-p chr) (char= #\. chr))))

(defun nthchar (lst a b)
  (when (and (>= a 0) (>= b 0) (< a (length lst)) (< b (length (nth a lst))))
    (char (nth a lst) b)))

(defun adjacent-symbols (lst l a b)
  (remove-if (complement #'symbol-char-p)
             `(,(nthchar lst l (1- a))
               ,(nthchar lst l b)
               ,@(loop :for i :from (1- a) :to b
                       :collect (nthchar lst (1- l) i) :into prev
                       :collect (nthchar lst (1+ l) i) :into next
                       :finally (return (concatenate 'list prev next))))))

(defun adjacent-symbol-p (lst l a b)
  (> (length (adjacent-symbols lst l a b)) 0))

(defun sum-parts (machine)
  (loop :for line :in machine
        :for line-num :from 0
        :sum (loop :for (num a b) :in (find-numbers line)
                       :when (adjacent-symbol-p machine line-num a b)
                     :sum num)))

(defun find-stars (line)
  (loop :for (a _) :on (ppcre:all-matches "\\*" line) :by #'cddr
        :collect a))

(defun adjacent-numbers (lst l n)
  (let ((nums (find-numbers (nth l lst)))
        (prev (find-numbers (and (> l 0) (nth (1- l) lst))))
        (next (find-numbers (nth (1+ l) lst))))
    (loop :for (num a b) :in (concatenate 'list prev nums next)
          :when (<= (1- a) n b)
            :collect num)))

(defun sum-gears (machine)
  (loop :for line :in machine
        :for line-num :from 0
        :for stars := (find-stars line)
        :when stars
          :sum (loop :for star :in stars
                     :for nums := (adjacent-numbers machine line-num star)
                     :when (eql 2 (length nums))
                       :sum (apply #'* nums))))

(defun solve-1 ()
  (with-open-file (in "./input.txt")
    (loop :for line := (read-line in nil nil)
          :while line
          :collect line :into machine
          :finally (return (sum-parts machine)))))

(defun solve-2 ()
  (with-open-file (in "./input.txt")
    (loop :for line := (read-line in nil nil)
          :while line
          :collect line :into machine
          :finally (return (sum-gears machine)))))
