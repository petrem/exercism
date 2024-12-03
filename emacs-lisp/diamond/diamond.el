;;; diamond.el --- Diamond (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun rows (letter)
  (reflect (mapcar (lambda (n) (row letter n)) (number-sequence 0 (- letter ?A))) 'vector))


(defun row (letter current)
  (let* ((offset (- letter ?A))
         (left-pad (- offset current))
         (right-pad current))
    (reflect (format (format "%%%ds%%c%%%ds" left-pad right-pad) "" (+ ?A current) ""))))
  
(defun reflect (sequence &optional type)
  (unless (null sequence)
    (seq-concatenate (or type (type-of sequence)) sequence (seq-rest (seq-reverse sequence)))))

(provide 'diamond)
;;; diamond.el ends here

