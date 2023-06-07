;;; leap.el --- Leap exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(defun leap-year-p (year)
  "Determine if YEAR is leap or not."
  (and
   (divisible-p year 4)
   (implies-p (divisible-p year 100) (divisible-p year 400))))

(defun divisible-p (x y)
  "Is X divisible by Y?"
  (= (mod x y) 0))

(defun implies-p (p q)
  "Return P -> Q."
  (or (not p) q))

(provide 'leap-year-p)
;;; leap.el ends here

