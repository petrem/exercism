(import (rnrs))

(define (collatz n)
  (if (<= n 0)
      (raise-exception 'not-striclty-positive)
      (let loop ((n n) (steps 0))
        (cond ((= n 1) steps)
              ((even? n) (loop (quotient n 2) (+ steps 1)))
              (else (loop (quotient (1+ (* n 3)) 2) (+ steps 2)))))))
               

