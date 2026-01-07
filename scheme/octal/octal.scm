(import (rnrs))

;; (define (to-decimal octal)
;;   (let ((char-set:octal-digit (string->char-set "01234567")))
;;     (guard (x [else 0])
;;       (string-fold
;;        (lambda (ch acc)
;;          (unless (char-set-contains? char-set:octal-digit ch)
;;            (display "boing\n")
;;            raise 'invalid-octal-digit)
;;          (+ (* 8 acc) (char->integer ch) (- (char->integer #\0))))
;;        0
;;        octal))))

(define (to-decimal octal)
  (let ((char-set:octal-digit (string->char-set "01234567")))
    (let octal-fold ((ds (string->list octal)) (acc 0))
      (cond ((null? ds)
             acc)
            ((char-set-contains? char-set:octal-digit (car ds))
             (octal-fold (cdr ds) (+ (* 8 acc) (char->integer (car ds)) (- (char->integer #\0)))))
            (else
             0)))))
