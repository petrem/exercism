(import (rnrs))

(define (atbash-encode text)
  (string-map (lambda (c) (if (char-set-contains? char-set:digit c)
                         c
                         (integer->char (+ 25 (* 2(char->integer #\a)) (- (char->integer c))))))
              text))

(define (encode phrase)
  (define (atbash-group text . group-len)
    (let ((group-len (if (null? group-len) 5 group-len))
          (text-len (string-length text)))
      (string-join (map (lambda (n) (string-copy text (* n group-len) (min (* (1+ n) group-len) text-len)))
                        (iota (ceiling (/ text-len group-len)))))))
  
  (atbash-group (atbash-encode (string-downcase (string-filter char-set:letter+digit phrase)))))

(define (decode phrase)
  (atbash-encode (string-filter char-set:letter+digit phrase)))
