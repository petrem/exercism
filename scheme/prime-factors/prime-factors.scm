(import (rnrs))

(use-modules (ice-9 match))

(define (factorize n)
  "Less naive approach."
  (match 
      (let while-divisible-by-2 ((n n) (k 0))
        (if (even? n)
            (while-divisible-by-2 (quotient n 2) (1+ k))
            `(,n . ,k)))
    ((odd-n . times-2)
     (let ((upper-limit (truncate (sqrt odd-n))))
       (let loop-to-sqrt-n ((k 3) (m odd-n) (factors (make-list times-2 2)))
         (cond ((> k upper-limit) (if (> m 2)
                                      (cons m factors)
                                      factors))
               ((zero? (remainder m k)) (loop-to-sqrt-n k (quotient m k) (cons k factors)))
               (else (loop-to-sqrt-n (+ k 2) m factors))))))))
