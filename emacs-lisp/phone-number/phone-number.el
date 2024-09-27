;;; phone-number.el --- phone-number Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;; This exericise definition is out of date.

;;; Code:

(defun numbers (num)
  (let ((digits (seq-filter (lambda (c) (not (seq-contains-p " .-()+" c))) num)))
    (if (and
         (seq-every-p (lambda (c) (<= ?0 c ?9)) digits)
         (or (length= digits 10) (and (length= digits 11) (= (car digits) ?1))))
        (concat (if (length= digits 10) digits (cdr digits)))
      "0000000000")))


(defun area-code (num)
  (concat (seq-take (numbers num) 3)))

(defun pprint (num)
  (apply #'format "(%s) %s-%s" (group-digits (numbers num))))

(defsubst group-digits (digits)
  (list
   (seq-subseq digits 0 3)
   (seq-subseq digits 3 6)
   (seq-subseq digits 6)))

(provide 'phone-number)
;;; phone-number.el ends here
