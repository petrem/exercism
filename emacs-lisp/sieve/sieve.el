;;; sieve.el --- Sieve (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;; Allowing a slight improvement on the stated algorithm, we iterate a bit less.

;;; Code:


(defun primes (limit)
  "List primes not greater than LIMIT."
  (let ((marks (make-bool-vector (1+ limit) t)))
    (aset marks 0 nil)
    (aset marks 1 nil)
    (dolist (i (number-sequence 2 (floor (sqrt limit))))
      (when (aref marks i)
        (dolist (j (number-sequence (* i i) limit i))
          (aset marks j nil))))
    (mapcar #'car
            (rassq-delete-all
             nil
             (seq-map-indexed (lambda (elt idx) `(,idx . ,elt)) marks)))))


(provide 'sieve)
;;; sieve.el ends here

