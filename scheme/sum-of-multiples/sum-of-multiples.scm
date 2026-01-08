(import (rnrs))

(define (sum-of-multiples ints limit)
  (define (multiples-up-to-limit k)
    (map (lambda (m) (* k (1+ m))) (iota (1- (ceiling (/ limit k))))))
  
  (define (dedup xs)
    (let loop ((xs xs) (acc '()))
      (cond ((null? xs) acc)
            ((null? (cdr xs)) (cons (car xs) acc))
            ((equal? (car xs) (cadr xs)) (loop (cdr xs) acc))
            (else (loop (cdr xs) (cons (car xs) acc))))))
  
  (apply +
         (dedup (sort (apply append
                             (map multiples-up-to-limit
                                  (filter (lambda (x) (> x 0))
                                          ints)))
                      <))))
         
