;;; anagram.el --- Anagram (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun anagrams-for (subject candidates)
  "Filter anagrams of SUBJECT in CANDIDATES."
  (let ((subject-lower (downcase subject)))
    (let ((subject-sorted (seq-sort '< subject-lower)))
      (seq-filter
       (lambda (x)
         (let ((x-lower (downcase x)))
           (and
            (not (string= x-lower subject-lower))
            (string= (seq-sort '< x-lower) subject-sorted))))
         candidates))))


(defun anagram--letter-counts (subject)
  "Letter counts of SUBJECT as alist."
  (seq-sort (lambda (alist1 alist2) (< (car alist1) (car alist2)))
            (mapcar
             (lambda (alist) (list (car alist) (1- (length alist))))
             (seq-group-by #'identity subject))))

(provide 'anagram)
;;; anagram.el ends here
