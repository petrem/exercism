(import (rnrs))

;; Not using formulas for the solutions on purpose. Live with it!

(define (square-of-sum n)
  (let ((sum (apply + (iota n 1))))
    (square sum)))

(define (sum-of-squares n)
  (apply + (map square (iota n 1))))

(define (difference-of-squares n)
  (- (square-of-sum n) (sum-of-squares n)))

(define (square n) (* n n))

;; I should have implemented iota myself to keep true to my goals of not using library
;; code, but meh, I don't think I'd really learn any Scheme along the way
