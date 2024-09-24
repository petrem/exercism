;;; yacht.el --- Yacht (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun score (scores category)
  "Compute score from dice SCORES for game CATEGORY."
  (pcase category
    ((or :ones :twos :threes :fours :fives :sixes)
     (* (cat-to-number category)
        (count-dice (cat-to-number category) scores)))
    ((or :yacht :four-of-a-kind)
     (pcase-let ((`(,which . ,how-many) (car (count-all scores))))
       (if (>= how-many (cat-to-number category))
           (if (equal category :yacht)
               50
             (* which 4))
         0)))
    (:choice (apply #'+ scores))
    (:full-house
     (pcase-let* ((counts (count-all scores))
                  (`(_ . ,c1) (car counts))
                  (`(_ . ,c2) (cadr counts)))
       (if (and (= c1 3) (= c2 2))
           (apply #'+ scores)
         0)))
    ((or :big-straight :little-straight)
     (let* ((from (cat-to-number category))
            (to (+ from 4)))
       (if (null (seq-difference (number-sequence from to) scores))
           30
         0)))
    (_ (error "Unknown category"))))

(defsubst cat-to-number (category)
  (alist-get category '((:ones . 1) (:twos . 2) (:threes . 3)
                        (:fours . 4) (:fives . 5) (:sixes . 6)
                        (:little-straight . 1 ) (:big-straight . 2)
                        (:yacht . 5) (:four-of-a-kind . 4))))

(defsubst count-dice (face dice)
  (seq-count (lambda (elt) (equal face elt)) dice))

(defun count-all (dice)
  (seq-sort-by #'cdr #'>
               (mapcar (lambda (face) `(,face . ,(count-dice face dice))) '(1 2 3 4 5 6))))

(provide 'yacht)
;;; yacht.el ends here
