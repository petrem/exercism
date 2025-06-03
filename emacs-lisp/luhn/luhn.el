;;; luhn.el --- Luhn (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defconst luhn-doubles '[0 2 4 6 8 1 3 5 7 9])

(defsubst odd-p (n) (/= (% n 2) 0))

(defun luhn-p (str)
  "Verify the number represented by STR is valid using the luhn checksum."
  (let* ((n-luhn-digits 0)
         (luhn-sum 0)
         (i (1- (length str))))
    (while (>= i 0)
      (let ((c (aref str i)))
        (cond
         ((<= ?0 c ?9)
          (let ((digit (- c ?0)))
            (setq luhn-sum (+ luhn-sum
                              (if (odd-p n-luhn-digits)
                                  (aref luhn-doubles digit)
                                digit))
                  n-luhn-digits (1+ n-luhn-digits))))
         ((/= c ? ) (error "Found character that is not digit or space: %c" c)))
        (setq i (1- i))))
    (if (< n-luhn-digits 2)
        nil
      (zerop (% luhn-sum 10)))))

(provide 'luhn)
;;; luhn.el ends here
