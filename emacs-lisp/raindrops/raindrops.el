;;; raindrops.el --- Raindrops (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(defun convert (n)
  "Convert integer N to its raindrops string."
  (let* ((as-rain (cl-reduce
                   (lambda (acc rain-word)
                     (cons (if (eq 0 (% n (car rain-word))) (cdr rain-word))
                           acc))
                   rainspeak :initial-value () ))
         (as-rain-str (string-join (reverse as-rain))))
    (if (string-empty-p as-rain-str)
        (number-to-string n)
      as-rain-str))
)

(defconst rainspeak '((3 . "Pling") (5 . "Plang") (7 . "Plong")))

(defsubst string-empty-p (string)
  "Check whether STRING is empty."
  (string= string ""))

(defsubst string-join (strings)
  "Join all STRINGS."
  (mapconcat #'identity strings nil))

(require 'cl-lib)
(require 'cl-seq)

(provide 'raindrops)
;;; raindrops.el ends here
