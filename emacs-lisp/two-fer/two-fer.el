;;; two-fer.el --- Two-fer Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun two-fer (&optional name)
  "Phrase two-fer depending on NAME."
  (let ((who (if (null name) "you" name)))
    (concat "One for " who ", one for me.")))


(provide 'two-fer)
;;; two-fer.el ends here
