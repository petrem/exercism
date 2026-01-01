(import (rnrs))

(define (proper-divisors n)
  (filter (lambda (m) (= (remainder n m) 0))
          (iota (- n 2) 1)))

(define (classify n)
  (cond ((<= n 0)
         (error #f "Not a positive natural number"))
        ((= n 1) 'deficient)
        (else
         (let ((aliquot-sum (apply + (proper-divisors n))))
           (cond ((= aliquot-sum n) 'perfect)
                 ((> aliquot-sum n) 'abundant)
                 ((< aliquot-sum n) 'deficient))))))
