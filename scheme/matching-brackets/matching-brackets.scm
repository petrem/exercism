(import (rnrs))

(use-modules (srfi srfi-1))  ; for alist-copy

(define (balanced? string)
  (let ((bracket-shape
         (lambda (bracket)
           (case bracket
             ((#\( #\)) 'round)
             ((#\[ #\]) 'square)
             ((#\{ #\}) 'curly))))
        (counts-update!
         (lambda (counts shape op)
           (assoc-set! counts shape (op (assoc-ref counts shape)))))
        (null-counts
         (alist-copy '((round . 0) (square . 0) (curly . 0))))
        (after-last-pos (string-length string)))
    (let loop ((counts null-counts) (par-stack '()) (pos 0))
      (if (= pos after-last-pos)
          (and (equal? counts null-counts)
               (null? par-stack))
          (let ((current (string-ref string pos)))
            (case current
              ((#\( #\[ #\{) (loop (counts-update! counts (bracket-shape current) 1+)
                                   (cons (bracket-shape current) par-stack)
                                   (1+ pos)))
              ((#\) #\] #\}) (if (and (not (null? par-stack))
                                      (equal? (bracket-shape current) (car par-stack)))
                                 (loop (counts-update! counts (bracket-shape current) 1-)
                                       (cdr par-stack)
                                       (1+ pos))
                                 ; unexpected closing bracket
                                 #f))
              (else (loop counts par-stack (1+ pos)))))))))
