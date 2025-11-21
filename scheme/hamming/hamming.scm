(import (rnrs))

(define (hamming-distance strand-a strand-b)
  (length (filter not (map eq? (string->list strand-a) (string->list strand-b)))))
