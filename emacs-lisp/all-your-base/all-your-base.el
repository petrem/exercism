;;; all-your-base.el --- All Your Base (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun rebase (list-of-digits in-base out-base)
  "Convert LIST-OF-DIGITS from base IN-BASE to OUT-BASE."
  (when (or (not (integerp in-base))
          (< in-base 2)
          (not (integerp out-base))
          (< out-base 2)
          (not (sequencep list-of-digits))
          (seq-some (lambda (elt) (or (not (integerp elt))
                                 (< elt 0)
                                 (>= elt in-base)))
                    list-of-digits))
    (error "Bad arguments"))
    (all-your-base--to-base out-base (all-your-base--from-base in-base list-of-digits)))

(defun all-your-base--from-base (in-base list-of-digits)
  "Compute the number formed of LIST-OF-DIGITS expressed in base IN-BASE."
  (seq-reduce (lambda (acc digit) (+ (* in-base acc) digit)) list-of-digits 0))

(defun all-your-base--to-base (out-base number)
  "Convert NUMBER to a list of digits in base OUT-BASE."
  (let ((result)
        (quot number))
    (while (> quot 0)
      (setq result (cons (% quot out-base) result))
      (setq quot (/ quot out-base)))
    (if (null result) '(0) result)))

(provide 'all-your-base)
;;; all-your-base.el ends here
