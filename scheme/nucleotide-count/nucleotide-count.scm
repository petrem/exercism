(import (rnrs))
(use-modules (srfi srfi-1))

(define (nucleotide-count dna)
  (string-fold
   (lambda (nucleotide counts)
     (case nucleotide
       ((#\A #\C #\G #\T) (assv-set! counts nucleotide (1+ (assv-ref counts nucleotide))))
       (else (raise-exception 'not-a-nucleotide))))
   (alist-copy '((#\A . 0) (#\C . 0) (#\G . 0) (#\T . 0)))
   dna))

