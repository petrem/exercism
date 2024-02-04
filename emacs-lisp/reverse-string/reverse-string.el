;;; reverse-string.el --- Reverse String (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;; Clearly weird way to solve this.

;;; Code:


(defun reverse-string (value)
  "Reverse VALUE."
  (seq-into (reverse-string--reverse-list (seq-into value 'list) ()) 'string))

(defun reverse-string--reverse-list (value result)
  (if (null value)
      result
    (reverse-string--reverse-list (cdr value) (cons (car value) result))))


(provide 'reverse-string)
;;; reverse-string.el ends here
