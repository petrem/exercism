(import (rnrs))

(define (knapsack capacity weights values)
  (let* ((capacity+1 (1+ capacity))
         (max-values-tbl (make-array 0 capacity+1)))
    
    (define (get-max-value-for-capacity)
      (array-ref max-values-tbl capacity))
    
    (let find-max-value-for ((weights weights) (values values))
      (when (not (or (null? weights) (null? values)))
          (let* ((weight (car weights))
                 (value (car values))
                 (update-from (min weight capacity+1)))
            (let update-max-values ((i capacity))
              (when (>= i update-from)
                (array-set! max-values-tbl
                            (max (array-ref max-values-tbl i)
                                 (+ value (array-ref max-values-tbl (- i weight))))
                            i)
                (update-max-values (1- i))))
            (find-max-value-for (cdr weights) (cdr values)))))
      (get-max-value-for-capacity)))

