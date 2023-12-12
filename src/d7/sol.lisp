(defstruct hand cards bid type)

(defun compare-hands-p (h1 h2 &key jokers)
  (let ((c1 (hand-cards h1))
        (t1 (hand-type h1))
        (c2 (hand-cards h2))
        (t2 (hand-type h2)))
    (cond
      ((< t1 t2) (return-from compare-hands-p t))
      ((> t1 t2) (return-from compare-hands-p nil))
      (:otherwise
       (let ((card-labels (if jokers "J23456789TQKA" "23456789TJQKA")))
         (loop :for a :across c1
               :for b :across c2
               :for ia = (position a card-labels)
               :for ib = (position b card-labels)
               :if (/= ia ib)
                 :do (return-from compare-hands-p (< ia ib))))))
    nil))

(defun solve-07-a (&key jokers)
  (let* ((input-lines (uiop:read-file-lines #p"input.txt"))
         (hands (extract-hands input-lines :jokers jokers)))
    (setf hands (sort hands (lambda (a b) (compare-hands-p a b :jokers jokers))))
    (loop :for rank :from 1 :to (length hands)
          :for h :in hands
          :sum (* rank (hand-bid h)))))

(defun solve-07-b ()
  (solve-07-a :jokers t))

(defun extract-hands (input-lines &key jokers)
  (loop :for line :in input-lines
        :for split = (str:split " " line)
        :for cards = (first split)
        :for bid = (parse-integer (second split))
        :collect (make-hand :cards cards :bid bid :type (categorize-cards cards :jokers jokers))))

(defun categorize-cards (cards &key jokers)
  (when (and jokers (string= cards "JJJJJ"))
    (return-from categorize-cards 7))
  (let* ((cards-alist (describe-cards cards))
         (joker-cards (assoc #\J cards-alist))
         (cards-wt-jokers (remove-if (lambda (pair) (char= (car pair) #\J)) cards-alist))
         (len (if jokers (length cards-wt-jokers) (length cards-alist)))
         (maximum (apply #'max (mapcar #'cdr (if jokers cards-wt-jokers cards-alist)))))
    (when (and jokers joker-cards)
      (incf maximum (cdr joker-cards)))
    (cond
      ((= maximum 5) 7)
      ((= maximum 4) 6)
      ((and (= maximum 3) (= len 2)) 5)
      ((and (= maximum 3) (= len 3)) 4)
      ((and (= maximum 2) (= len 3)) 3)
      ((= len 4) 2)
      ((= len 5) 1))))

(defun describe-cards (cards)
  (let ((cards-alist ()))
    (loop :for c :across cards
          :for c-pair = (assoc c cards-alist :test #'char=)
          :if c-pair
            :do (incf (cdr c-pair))
            :else :do (push `(,c . 1) cards-alist))
    cards-alist))
