;;; acronym.el --- Acronym (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun acronym (phrase)
  "Create acronym from PHRASE."
  (let ((match-pos 0)
        (acronym '())
        (acro-syntax (make-syntax-table)))
    (modify-syntax-entry ?' "w" acro-syntax)
    (with-syntax-table acro-syntax
      (while (string-match "\\<\\w" phrase match-pos)
        (push (match-string 0 phrase) acronym)
        (setq match-pos (1+ (match-end 0))))
      (mapconcat #'upcase (reverse acronym)))))

(provide 'acronym)
;;; acronym.el ends here
