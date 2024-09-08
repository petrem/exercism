;;; allergies.el --- Allergies Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;; Exploring usage of generators and macros.

;;; Code:

(require 'generator)

(defun allergen-list (score)
  "List the allergens in SCORE using generator."
  (let (list)
    (iter-do (flag (allergies--iter-ones score))
      (allergies--let-if allergen
                         (rassoc flag allergies--allergens)
                         (push (car allergen) list)))
    (reverse list)))


(defun allergic-to-p (score allergen)
  "Determine if ALLERGEN is present in SCORE."
  (seq-contains-p (allergen-list score) allergen))


(defconst allergies--allergens
  '(("eggs" . 1) ("peanuts" . 2) ("shellfish" . 4) ("strawberries" . 8)
    ("tomatoes" . 16) ("chocolate" . 32) ("pollen" . 64) ("cats" . 128))
  "List of known allergens.")

(defmacro allergies--let-if (var expr1 expr2)
  "Evaluate EXPR1 as VAR and conditionally evaluate EXPR2 that may contain VAR.

The idea is to evaluate EXPR1 only once."
  `(let ((,var ,expr1)) (if ,var ,expr2)))


(iter-defun allergies--iter-ones (integer)
  "Iterate over the 1 bits of INTEGER, from the right."
  (setq shift 1)
  (while (>= integer shift)
    (if (/= (logand integer shift) 0) (iter-yield shift))
    (setq shift (lsh shift 1))))


(provide 'allergies)
;;; allergies.el ends here
