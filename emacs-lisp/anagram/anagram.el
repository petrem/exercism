;;; anagram.el --- Anagram (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun anagrams-for (subject candidates)
  "Filter anagrams of SUBJECT in CANDIDATES."
  (let ((subject-lower (downcase subject)))
    (let ((subject-sorted (seq-sort '< subject-lower)))
      (seq-filter
       (lambda (candidate)
         (let ((candidate-lower (downcase candidate)))
           (and
            (not (string= candidate-lower subject-lower))
            (string= (seq-sort '< candidate-lower) subject-sorted))))
         candidates))))

(provide 'anagram)
;;; anagram.el ends here
