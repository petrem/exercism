;;; reverse-string.el --- Reverse String (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defsubst reverse-string--swap (arr idx1 idx2)
  "Swap elements at IDX1 and IDX2 in ARR."
  (let ((tmp (aref arr idx1)))
    (aset arr idx1 (aref arr idx2))
    (aset arr idx2 tmp)))

(defun reverse-string (value)
  "Reverse VALUE."
  (let* ((copy (copy-sequence value))
         (l-idx 0)
         (r-idx (1- (length value))))
    (while (< l-idx r-idx)
      (reverse-string--swap copy l-idx r-idx)
      (setq l-idx (1+ l-idx))
      (setq r-idx (1- r-idx)))
    copy))

(provide 'reverse-string)
;;; reverse-string.el ends here
