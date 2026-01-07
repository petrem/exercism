(import (rnrs))

(define (keep pred seq)
  (let loop ((seq seq) (res '()))
    (cond ((null? seq) (reverse res))
          ((pred (car seq)) (loop (cdr seq) (cons (car seq) res)))
          (else (loop (cdr seq) res)))))

(define (discard pred seq)
  (keep (negate pred) seq))
