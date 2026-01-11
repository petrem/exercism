(import (rnrs))

(define (square n)
  (when (or (< n 1) (> n 64))
    (error "Wrong input"))
  ;; TODO: find out the bit shift operation
  (if (eq? n 1)
      1
    (* 2 (square (1- n)))))

(define total
  #xffffffffffffffff)

