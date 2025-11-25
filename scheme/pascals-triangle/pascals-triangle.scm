(import (rnrs))

(define (pascals-triangle n)
  (let build-rows ((m (1- n)) (result '()))
    (if (< m 0)
        result
        (build-rows
         (1- m)
         (cons
          (let build-row ((k 0) (row '(1)))
            (if (>= k m)
                row
                (build-row
                 (1+ k)
                 (cons (/ (* (car row) (- m k)) (1+ k))
                       row))))
          result)))))
