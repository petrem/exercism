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

  (define (classify)
    (let ((trimmed-message (string-trim-both message)))
     (cond ((silence? trimmed-message) 'Silence)
          ((yell-question? trimmed-message) 'Yell-Question)
          ((question? trimmed-message) 'Question)
          ((yell? trimmed-message) 'Yell)
          (else 'Other))))

  (define lackadaisical-answers
    '((Silence . "Fine. Be that way!")
      (Yell-Question . "Calm down, I know what I'm doing!")
      (Question . "Sure.")
      (Yell . "Whoa, chill out!")
      (Other . "Whatever.")))

  (assq-ref lackadaisical-answers (classify)))
