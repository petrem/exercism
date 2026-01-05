(import (rnrs))

(define (rotate phrase dx)
  (define repeated-lowercase "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz")
  (define repeated-uppercase "ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ")
  (define (rotate-char c lookup)
    (string-ref lookup (+ dx (- (char->integer c) (char->integer (string-ref lookup 0))))))
  (string-map (lambda (c)
                (cond ((char-set-contains? char-set:upper-case c) (rotate-char c repeated-uppercase))
                      ((char-set-contains? char-set:lower-case c) (rotate-char c repeated-lowercase))
                      (else c)))
                phrase))

