(import (rnrs))

(define (to-decimal trinary)
  (let ((char-set:trinary-digit (string->char-set "012")))
    (let trinary-fold ((ds (string->list trinary)) (acc 0))
      (cond ((null? ds)
             acc)
            ((char-set-contains? char-set:trinary-digit (car ds))
             (trinary-fold (cdr ds) (+ (* 3 acc) (char->integer (car ds)) (- (char->integer #\0)))))
            (else
             0)))))
