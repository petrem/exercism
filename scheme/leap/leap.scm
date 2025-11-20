(import (rnrs))

(define (leap-year? year)
  
  (define (divisible-by? n) (eq? (remainder year n) 0))
  
  (and (divisible-by? 4)
       (or (not (divisible-by? 100)) (divisible-by? 400))))

