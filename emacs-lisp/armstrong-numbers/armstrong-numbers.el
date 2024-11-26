;;; armstrong-numbers.el --- armstrong-numbers Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun armstrong-p (n)
  (let* ((digits (mapcar (lambda (d) (- d ?0)) (number-to-string n)))
         (n_digits (length digits))
         (sum-digits (apply #'+ (mapcar (lambda (d) (expt d n_digits)) digits))))
    (= n sum-digits)))


(provide 'armstrong-numbers)
;;; armstrong-numbers.el ends here
