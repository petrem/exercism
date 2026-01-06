"How is this 'accumulate' ?!"

(define (accumulate f xs)
  (reverse
   (let loop ((xs xs) (result '()))
     (if (null? xs)
         result
         (loop (cdr xs) (cons (f (car xs)) result))))))
