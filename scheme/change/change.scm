(import (rnrs))

"A top-down DP with memoization. Several things are sub-optimal, to be revisited."

(define (change amount denominations)
  (let* ((lookup '())
         (solution
          (let loop ((target amount))
            (cond ((= target 0) '())
                  ((assv-ref lookup target) (assv-ref lookup target))
                  (else (let* ((useful-coins (filter (lambda (c) (<= c target)) denominations))
                               (try-each-useful-coin
                                (map (lambda (c) (let ((sub-solution (loop (- target c))))
                                              (if (eq? sub-solution 'undefined)
                                                  'undefined
                                                  (cons c sub-solution))))
                                     useful-coins))
                               (shortest (min-by-length try-each-useful-coin)))
                          (set! lookup (assv-set! lookup target shortest))
                          shortest))))))
    (if (eq? solution 'undefined)
        (raise-exception 'no-solution)
        solution)))

(define (min-by-length lists)
  "Return shortest list in LISTS."
  (let loop ((lists lists) (shortest 'undefined))
    (cond ((null? lists) shortest)
          ((eq? shortest 'undefined) (loop (cdr lists) (car lists)))
          ((eq? (car lists) 'undefined) (loop (cdr lists) shortest))
          ((< (length (car lists)) (length shortest)) (loop (cdr lists) (car lists)))
          (else (loop (cdr lists) shortest)))))

