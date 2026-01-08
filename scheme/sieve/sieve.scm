(import (rnrs)
        (rnrs arithmetic bitwise))

(define (sieve limit)
  (let ((marks (make-bitvector (1+ limit) #t)))
    (bitvector-clear-bit! marks 0)
    (bitvector-clear-bit! marks 1)
    (do ((i 2 (1+ i))) ((> i (floor (sqrt limit))))
      (when (bitvector-bit-set? marks i)
        (do ((j (* i i) (+ j i))) ((> j limit)) 
          (bitvector-clear-bit! marks j))))
    (let loop ((prev-idx 2) (primes '()))
      (let ((found-idx (bitvector-position marks #t prev-idx)))
        (if found-idx
            (loop (1+ found-idx) (cons found-idx primes))
            (reverse primes))))))


