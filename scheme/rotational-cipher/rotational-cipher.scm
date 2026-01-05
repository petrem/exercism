(import (rnrs))

(define (rotate phrase dx)
  (define repeated-lowercase "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz")
  (define repeated-uppercase "ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ")
  (define (rotate-char c lookup)
    (string-ref lookup (+ dx (- (char->integer c) (char->integer (string-ref lookup 0))))))
  (string-map (lambda (c)
                (cond ((char-upper-case? c) (rotate-char c repeated-uppercase))
                      ((char-lower-case? c) (rotate-char c repeated-lowercase))
                      (else c)))
                phrase))

