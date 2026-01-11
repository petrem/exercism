(import (rnrs))

(define (score word)
  (apply + (map char-score (string->list (string-upcase word)))))

(define (char-score c)
  (case c
    ((#\A #\E #\I #\L #\N #\O #\R #\S #\T #\U) 1)
    ((#\D #\G) 2)
    ((#\B #\C #\M #\P) 3)
    ((#\F #\H #\V #\W #\Y) 4)
    ((#\K) 5)
    ((#\J #\X) 8)
    ((#\Q #\Z) 10)))
