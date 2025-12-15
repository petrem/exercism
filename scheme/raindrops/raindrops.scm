(import (rnrs))

(define (convert number)
  (let ((rainspeak
         (apply string-append
                (map (lambda (rainword)
                       (if (divisible number (rainword-number rainword))
                           (rainword-sound rainword)
                           ""))
                     rainwords))))
    (if (string-null? rainspeak)
        (number->string number)
        rainspeak)))

(define rainwords '((3 . "Pling")
                    (5 . "Plang")
                    (7 . "Plong")))

(define rainword-number car)
(define rainword-sound cdr)
(define (divisible n m) (= (remainder n m) 0))
