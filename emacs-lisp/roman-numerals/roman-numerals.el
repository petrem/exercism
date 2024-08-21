;;; roman-numerals.el --- roman-numerals Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defmacro roman-numerals--make-digits (one five ten)
  "Make a vector of roman digits from zero to ten.
It uses ONE, FIVE and TEN to infer the other digits, e.g.
\(roman-numerals--make-digits ?X ?L ?C)
    => [\"\" \"X\" \"XX\" ... \"XC\" \"C\"]"
  `(vconcat (mapcar 'concat
                    '(() (,one) (,one ,one) (,one ,one ,one)
                      (,one ,five) (,five) (,five ,one) (,five ,one ,one)
                      (,five ,one ,one ,one) (,one ,ten) (,ten)))))

(defconst roman-numerals--digits
  (vector (roman-numerals--make-digits ?I ?V ?X)  ;; 0: I..X
          (roman-numerals--make-digits ?X ?L ?C)  ;; 1: X..C
          (roman-numerals--make-digits ?C ?D ?M)  ;; 2: C..M
          ["" "M" "MM" "MMM"])                     ;; 3: M..MMM
  "Roman digits from 0 to 10 for singles, tens, hundreds and thousands.")

(defun to-roman (value)
  "Convert VALUE to roman numerals string."
  (concat (aref (aref roman-numerals--digits 3) (/ value 1000))
          (aref (aref roman-numerals--digits 2) (/ (% value 1000) 100))
          (aref (aref roman-numerals--digits 1) (/ (% value 100) 10))
          (aref (aref roman-numerals--digits 0) (% value 10))))


(provide 'roman-numerals)
;;; roman-numerals.el ends here
