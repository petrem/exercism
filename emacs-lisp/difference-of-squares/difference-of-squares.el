;;; difference-of-squares.el --- Difference of Squares (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:
; There should be a prettier way to write long math formulas.

;;; Code:

(defun sum-of-squares (n)
  "Calculate sum of squares of 1..N."
  (/
   (*
    n
    (+ n 1)
    (+
     (* n 2)
     1))
   6)
)

(defun square-of-sum (n)
  "Calculate square of sum of 1..N."
  (let ((sum (/ (* n (+ n 1)) 2)))
    (* sum sum))
)

(defun difference (n)
  "Calculate difference of square-of-sum and sum-of-squares for N."
  (-
   (square-of-sum n)
   (sum-of-squares n))
)

(provide 'difference-of-squares)
;;; difference-of-squares.el ends here
