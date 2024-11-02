;;; spiral-matrix.el --- Spiral Matrix (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defsubst vpad (l m r) (vconcat `[,l] m `[,r]))

(defun spiral-matrix (size &optional from)
  (let ((from (or from 1)))
    (pcase size
      (0 [])
      (1 `[[,from]])
      (_ (let* ((top-right    (+ from         size -1))
                (bottom-right (+ top-right    size -1))
                (bottom-left  (+ bottom-right size -1))
                (top-left     (+ bottom-left  size -1))
                (top          (seq-into (number-sequence from           top-right          ) 'vector))
                (bottom       (seq-into (number-sequence bottom-left    bottom-right     -1) 'vector))
                (left         (seq-into (number-sequence (1- top-left)  (1+ bottom-left) -1) 'vector))
                (right        (seq-into (number-sequence (1+ top-right) (1- bottom-right)  ) 'vector))
                (inner        (spiral-matrix (- size 2) top-left))
                (middle       (seq-into (seq-mapn #'vpad left inner right) 'vector)))
           (vpad top middle bottom))))))

(provide 'spiral-matrix)
;;; spiral-matrix.el ends here
