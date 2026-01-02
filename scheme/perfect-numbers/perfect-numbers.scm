(import (rnrs))

(define (proper-divisors n)
  (let* ((int-sqrt (floor (sqrt n)))
         (divisors-up-to-sqrt
          (let loop ((k 2) (divisors '()))
            (if (<= k int-sqrt)
                (loop (1+ k)
                      (if (zero? (remainder n k))
                          (cons k divisors)
                          divisors))
                divisors))))
    (if (null? divisors-up-to-sqrt)
        '(1)
        (let* ((paired-divisors
                (map (lambda (divisor) (quotient n divisor)) divisors-up-to-sqrt))
               (paired-divisors-without-sqrt
                (if (= (car divisors-up-to-sqrt) (car paired-divisors))
                    (cdr paired-divisors)
                    paired-divisors)))
          (append '(1) divisors-up-to-sqrt paired-divisors-without-sqrt)))))

(define (classify n)
  (cond ((<= n 0) (error #f "Not a positive natural number"))
        ((= n 1) 'deficient)
        (else
         (let* ((aliquot-sum (apply + (proper-divisors n))))
           (cond ((= aliquot-sum n) 'perfect)
                 ((> aliquot-sum n) 'abundant)
                 ((< aliquot-sum n) 'deficient))))))
