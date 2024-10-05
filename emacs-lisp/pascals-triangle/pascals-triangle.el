;;; pascals-triangle.el --- Pascal&#39;s Triangle (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun rows (count)
  (if (<= count 0)
      []
    (let* ((prev '[1])
           (rows (vector prev)))
      (dotimes (_ (1- count) rows)
        (let* ((next (apply
                      #'vector
                      (seq-mapn #'+ (vconcat [0] prev) (vconcat prev [0])))))
          (setq prev next)
          (setq rows (vconcat rows (vector next))))))))
(provide 'pascals-triangle)
;;; pascals-triangle.el ends here

