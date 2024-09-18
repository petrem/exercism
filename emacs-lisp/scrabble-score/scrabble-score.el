;;; scrabble-score.el --- Scrabble Score (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun score (word)
  "Calculate scrabble score of WORD."
  (apply #'+
         (mapcar
          (lambda (l) (alist-get l scrabble-score--scores))
          (upcase word))))

(defmacro scrabble-score--alist-for (letters score)
  "Create alist mapping each of LETTERS to SCORE."
  `',(mapcar (lambda (l) `(,l . ,score)) letters))

(defconst scrabble-score--scores
  (append
   (scrabble-score--alist-for "AEIOULNRST" 1)
         (scrabble-score--alist-for "DG" 2)
         (scrabble-score--alist-for "BCMP" 3)
         (scrabble-score--alist-for "FHVWY" 4)
         (scrabble-score--alist-for "K" 5)
         (scrabble-score--alist-for "JX" 8)
         (scrabble-score--alist-for "QZ" 10))
  "Alist mapping upper-case letters to scores.")

(provide 'scrabble-score)
;;; scrabble-score.el ends here
