(import (rnrs))

(define (square n)
  (when (or (< n 1) (> n 64))
    (error "Wrong input"))
  (let loop ((n n) (acc 1))
    (if (= n 1)
        acc
        (loop (1- n) (ash acc 1)))))

(define total
  #xffffffffffffffff)

