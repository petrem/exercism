(import (rnrs))

(define (response-for message)
  (define (question? s)
    (string-suffix? "?" s))

  (define (yell? s)
    (and (not (string-any char-lower-case? s))
         (string-any char-upper-case? s)))

  (define (yell-question? s)
    (and (yell? s) (question? s)))

  (define (silence? s)
    (string-null? s))

  (let ((trimmed-message (string-trim-both message)))
    (cond ((silence? trimmed-message) "Fine. Be that way!")
          ((yell-question? trimmed-message) "Calm down, I know what I'm doing!")
          ((question? trimmed-message) "Sure.")
          ((yell? trimmed-message) "Whoa, chill out!")
          (else "Whatever."))))
