(import (rnrs))

(define (anagram target words)
  (let ((target-letters (sort (string-downcase target) char<?)))
    (filter (lambda (word)
              (and (string=? target-letters (sort (string-downcase word) char<?))
                   (not (string-ci=? target word))))
                   
            words)))
    
