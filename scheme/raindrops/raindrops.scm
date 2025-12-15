(import (rnrs))

(define (convert number)
  (define (make-sound rainword)
    (if (divisible-by? number (rainword-number rainword))
        (rainword-sound rainword)
        ""))
  (let ((rainspeak (string-concatenate (map make-sound rainwords))))
    (if (string-null? rainspeak)
        (number->string number)
        rainspeak)))

(define rainwords '((3 . "Pling")
                    (5 . "Plang")
                    (7 . "Plong")))

(define rainword-number car)
(define rainword-sound cdr)
(define (divisible-by? n m) (= (remainder n m) 0))
