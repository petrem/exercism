"How is this 'accumulate' ?!"

(define (accumulate f xs)
  (let loop ((xs xs) (result '()))
    (if (null? xs)
        (reverse result)
        (loop (cdr xs) (cons (f (car xs)) result)))))
