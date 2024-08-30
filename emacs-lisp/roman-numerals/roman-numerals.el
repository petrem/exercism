;;; roman-numerals.el --- roman-numerals Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun to-roman (value)
  "Convert VALUE to roman numerals string."
  (seq-reduce
   (lambda (acc elem) (concat acc (roman-numerals--digits-for value (car elem) (cdr elem))))
   roman-numerals--digits
   ""))


(defmacro roman-numerals--make-digits (one five ten)
  "Make a vector of roman digits from zero to ten.
It uses ONE, FIVE and TEN to infer the other digits, e.g.
\(roman-numerals--make-digits ?X ?L ?C)
    => [\"\" \"X\" \"XX\" ... \"XC\" \"C\"]

Note to self: I'm keeping this as is for <3 reasons as my first macro."
  `(vconcat (mapcar 'concat
                    '(() (,one) (,one ,one) (,one ,one ,one)
                      (,one ,five) (,five) (,five ,one) (,five ,one ,one)
                      (,five ,one ,one ,one) (,one ,ten) (,ten)))))

(defconst roman-numerals--digits
  `((3 . ["" "M" "MM" "MMM"])
    (2 . ,(roman-numerals--make-digits ?C ?D ?M))
    (1 . ,(roman-numerals--make-digits ?X ?L ?C))
    (0 . ,(roman-numerals--make-digits ?I ?V ?X)))
  "Roman digits from 0 to 10 for singles, tens, hundreds and thousands.")


(defun roman-numerals--digits-for (number position roman-digits)
  "Choose from ROMAN-DIGITS the one corresponding to NUMBER at POSITION."
  (aref roman-digits (/ (% number (expt 10 (1+ position))) (expt 10 position))))

(provide 'roman-numerals)
;;; roman-numerals.el ends here
