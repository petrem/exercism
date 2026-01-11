(import (rnrs))

(define (dna->rna dna)
  (let ((dna-nucleotide->rna-nucleotide
         (lambda (c)
           (case c
             ((#\A) #\U)
             ((#\C) #\G)
             ((#\G) #\C)
             ((#\T) #\A)))))
    (string-map dna-nucleotide->rna-nucleotide dna)))
