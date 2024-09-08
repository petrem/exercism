;;; allergies.el --- Allergies Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;; Second version: Using an unfoldr (aka builder) pattern in 'allergen-list',
;; and separately, a more classic approach for 'allergic-to-p'.

;;; Code:

(defun allergen-list (score)
  "List the allergens in SCORE using generator."
  (reverse
   (allergies--unfoldr
    (lambda (pow2)
      (when (and pow2 (<= pow2 129))
          (cons (car (rassoc pow2 allergies--allergens))
                (allergies--next-set-bit-from score pow2))))
    (allergies--next-set-bit-from score 0))))

(defun allergic-to-p (score allergen)
  "Determine if ALLERGEN is present in SCORE."
  (/= 0 (logand score (cdr (assoc allergen allergies--allergens)))))


(defconst allergies--allergens
  '(("eggs" . 1) ("peanuts" . 2) ("shellfish" . 4) ("strawberries" . 8)
    ("tomatoes" . 16) ("chocolate" . 32) ("pollen" . 64) ("cats" . 128))
  "List of known allergens.")

(defun allergies--unfoldr (fun init)
  "Builds a list using a function FUN and an INIT-ial element.

FUN should take an argument of the type of INIT and return a cons
cell containing the new item for the constructed list in its car
and a new element for the next iteration. When there is no next
item to be generated, it must return nil."
  (let ((next (funcall fun init))
        list)
    (prog2
        (while (consp next)
          (push (car next) list)
          (setq next (funcall fun (cdr next))))
        list)))

(defun allergies--next-set-bit-from (number pow2from)
  "Find next power of two where a bit set in NUMBER starting at POW2FROM."
  (let ((next (if (zerop pow2from) 1 (lsh pow2from 1))))
    (while (and (>= number next) (zerop (logand number next)))
      (setq next (lsh next 1)))
    (if (< number next)
        nil
      next)))

(provide 'allergies)
;;; allergies.el ends here
