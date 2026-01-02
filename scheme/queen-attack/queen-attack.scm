(import (rnrs))

(define (attacking? white black)
  (or (= (car white) (car black))
      (= (cadr white) (cadr black))
      (= (abs (- (car white) (car black)))
         (abs (- (cadr white) (cadr black))))))

