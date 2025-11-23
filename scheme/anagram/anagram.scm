(import (rnrs))

;; Changed inner helper function in `list-counter` with a recursive-let.

(define (anagram target words)
  (filter (anagram-checker-for target) words))

(define (anagram-checker-for target)
  (let* ((target-downcased (string-downcase target))
         (target-counts (string-counter target-downcased)))
    (lambda (word)
      (and (not (string-ci=? target word))
           (counter= target-counts (string-counter (string-downcase word)))))))

(define (counter= lhs rhs)
  (equal? (assoc-sort lhs char<?) (assoc-sort rhs char<?)))

(define (string-counter word)
  (list-counter (string->list word)))

(define (list-counter lst)
  (let go ((elems lst) (counts '()))
    (if (null? elems)
        counts
        (let* ((head (car elems))
               (head-count (1+ (assv-ref-default counts head 0))))
          (go (cdr elems) (assv-set! counts head head-count))))))

(define (assv-ref-default alist key default)
  (let ((ent (assv key alist)))
    (or (and ent (cdr ent)) default)))

(define (assoc-sort alist less)
  (sort-list alist
             (lambda (lhs rhs)
               (less (car lhs) (car rhs)))))
