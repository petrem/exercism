;;; luhn.el --- Luhn (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun luhn-p (str)
  "Verify the number represented by STR is valid using the luhn checksum."
  (let* ((flipflop t)
         (luhn-digits
          (mapcar
           (lambda (char)
             (let* ((digit (- char ?0))
                    (doubled (* digit 2))
                    (luhn-digit (if (> doubled 9) (- doubled 9) doubled)))
               (setq flipflop (not flipflop))
               (if flipflop luhn-digit digit)))
           (reverse
            (seq-filter
             (lambda (c)
               (cond
                ((<= ?0 c ?9) t)
                ((= c ? ) nil)
                (t (error "Non-digit or space found: %c" c))))
             str)))))
    (if (length< luhn-digits 2)
        nil
      (zerop (% (apply #'+ luhn-digits) 10)))))

(provide 'luhn)
;;; luhn.el ends here
