;;; binary-search.el --- Binary Search (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun find-binary (array value)
  (defun binary-search--find (left right)
    (when (<= left right)
      (let* ((middle-index (/ (+ left right) 2))
             (middle (aref array middle-index)))
        (cond ((equal value middle) middle-index)
              ((< value middle) (binary-search--find left (1- middle-index)))
              ((> value middle) (binary-search--find (1+ middle-index) right))))))
  (binary-search--find 0 (1- (seq-length array))))



(provide 'binary-search)
;;; binary-search.el ends here
