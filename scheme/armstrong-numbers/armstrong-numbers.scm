(import (rnrs))

(define (armstrong-number? n)
  (let* ((digits (map (lambda (d) (- (char->integer d) (char->integer #\0)))
                      (string->list (number->string n))))
         (n-digits (length digits))
         (armstrong-sum (apply + (map (lambda (d) (expt d n-digits)) digits))))
    (= armstrong-sum n)))
