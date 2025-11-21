(import (rnrs))

(define (dna->rna dna)
  (define (dna-nucleotide->rna-nucleotide c)
    (case c
      ((#\A) #\U)
      ((#\C) #\G)
      ((#\G) #\C)
      ((#\T) #\A)))
  (string-map dna-nucleotide->rna-nucleotide dna))
