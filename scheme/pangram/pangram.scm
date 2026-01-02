(import (rnrs))

(define (pangram? phrase)
  (let ((letters-flags (make-bitvector 26 #f)))
    (let loop ((pos 0) (letters-found 0))
      (cond
       ((= letters-found 26) #t)
       ((>= pos (string-length phrase)) #f)
       ((not (char-alphabetic? (string-ref phrase pos))) (loop (1+ pos) letters-found))
       (else
        (let ((letter-index (- (char->integer (char-upcase (string-ref phrase pos))) (char->integer #\A))))
          (if (bitvector-bit-set? letters-flags letter-index)
              (loop (1+ pos) letters-found)
              (begin (bitvector-set-bit! letters-flags letter-index)
                     (loop (1+ pos) (1+ letters-found))))))))))

