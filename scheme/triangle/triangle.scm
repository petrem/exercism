"The tests on this track seem to consider degenerate triangles are not triangles, what a shame ;-)"

(import (rnrs))

(define (triangle a b c)
  (define (triangle?)
    (and (> a 0)
         (> b 0)
         (> c 0)
         (> (+ a b) c)
         (> (+ a c) b)
         (> (+ b c) a)))
  (unless (triangle?) (raise-exception 'not-a-triangle))
  (let ((equal-sides-number
         (length (filter (lambda (x) x)
                         (map (lambda (pair) (apply eqv? pair)) `((,a ,b) (,a ,c) (,b ,c)))))))
    (cond ((= equal-sides-number 3) 'equilateral)
          ((> equal-sides-number 0) 'isosceles)
          (else 'scalene))))
