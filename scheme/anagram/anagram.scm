(import (rnrs))

(define (anagram target words)
  (let ((target-letters (sort (string-downcase target) char<?)))
    (filter (lambda (word)
              (and (not (string-ci=? target word))
                   (string=? target-letters (sort (string-downcase word) char<?))))
            words)))
    
