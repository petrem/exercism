;;; isogram.el --- isogram (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun isogramp (phrase)
  "Check whether PHRASE is an isogram."
  (let ((cleaned (seq-filter (lambda (c) (<= ?a c ?z)) (downcase phrase))))
    (equal (seq-uniq cleaned) cleaned)))


(provide 'isogram)
;;; isogram.el ends here
