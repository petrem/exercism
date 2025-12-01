(import (rnrs))

(define (knapsack capacity weights values)
  (let* ((capacity+1 (1+ capacity))
         (row (make-array 0 capacity+1)))
    (for-each
     (lambda (weight value)
       (display (format #f "~s ~s: ~s\n" weight value row))
       (let ((update-from (min weight capacity+1))
             (row-copy (make-array 0 capacity+1)))
         (array-copy! row row-copy)
         (for-each
          (lambda (i)
            (array-set!
             row-copy
             (max (array-ref row i) (+ value (array-ref row (- i weight))))
             i))
          (range update-from capacity+1))
         (set! row row-copy)))
     weights
     values)
    (display (format #f "last: ~s\n" row))
    (array-ref row capacity)))

(define (range a b) (iota (max 0 (- b a)) a))


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
                   
