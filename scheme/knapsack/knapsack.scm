(import (rnrs))

(define (knapsack capacity weights values)
  (let* ((capacity+1 (1+ capacity))
         (max-values-tbl (make-array 0 capacity+1)))
    (define (find-max-value-for weight value)
      (let ((update-from (min weight capacity+1)))
        (let update-max-values ((i capacity))
          (when (>= i update-from)
            (array-set! max-values-tbl
                        (max (array-ref max-values-tbl i)
                             (+ value (array-ref max-values-tbl (- i weight))))
                        i)
            (update-max-values (1- i))))))
    (define (get-max-value-for-capacity) (array-ref max-values-tbl capacity))

    (for-each find-max-value-for weights values)
    (get-max-value-for-capacity)))


;; Initial recursive solution, expectedly slow.
;; It would probably work if I could memoize the results.
       
;; (define (knapsack capacity weights values)
;;   (let get-max-value ((max-value 0)
;;                       (value 0)
;;                       (capacity capacity)
;;                       (weights weights)
;;                       (values values))
;;     ;;(display (format #f "~s ~s ~s ~s ~s\n" max-value value capacity weights values))
;;     (if (null? weights)
;;         (max max-value value)
;;         (let ((item-weight (car weights))
;;               (item-value (car values))
;;               (rest-weights (cdr weights))
;;               (rest-values (cdr values)))
;;           (get-max-value
;;            (if (> item-weight capacity)
;;                max-value
;;                (max max-value
;;                     (get-max-value max-value
;;                                    (+ value item-value)
;;                                    (- capacity item-weight)
;;                                    rest-weights
;;                                    rest-values)))
;;            value
;;            capacity
;;            rest-weights
;;            rest-values)))))
                   
