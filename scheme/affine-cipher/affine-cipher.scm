(import (rnrs))

(define (encode key text)
  (unless (= 1 (gcd (car key) 26))
    (raise-exception 'key-part-a-not-coprime-with-26))

  (define (encode-char ch)
    (index->letter (modulo (+ (* (car key) (letter->index ch))
                                 (cdr key))
                              26)))

  (let fold-text ((pos 0) (acc '()))
    (if (>= pos (string-length text))
        (group (list->string (reverse acc)))
        (let ((ch (string-ref text pos)))
          (cond ((char-set-contains? char-set:letter ch)
                 (fold-text (1+ pos) (cons (encode-char (char-downcase ch)) acc)))
                ((char-set-contains? char-set:digit ch)
                 (fold-text (1+ pos) (cons ch acc)))
                (else
                 (fold-text (1+ pos) acc)))))))

(define (decode key text)
  (define-values (mmi-a-26 gcd-a-26)
    (multiplicative-modular-inverse (car key)
                                    26))
  (unless (= 1 gcd-a-26)
    (raise-exception 'key-part-a-not-coprime-with-26))

  (define (decode-char ch)
    (index->letter (modulo (* mmi-a-26
                                 (- (letter->index ch) (cdr key)))
                              26)))
  (let fold-text ((pos 0) (acc '()))
    (if (>= pos (string-length text))
        (list->string (reverse acc))
        (let ((ch (string-ref text pos)))
          (cond ((char-lower-case? ch)
                 (fold-text (1+ pos) (cons (decode-char ch) acc)))
                ((char-numeric? ch)
                 (fold-text (1+ pos) (cons ch acc)))
                (else
                 (fold-text (1+ pos) acc)))))))
                 

(define (letter->index ch)
  (- (char->integer ch) (char->integer #\a)))

(define (index->letter i)
  (integer->char (+ i (char->integer #\a))))

 (define (group text . group-len)
    (let ((group-len (if (null? group-len) 5 group-len))
          (text-len (string-length text)))
      (string-join (map (lambda (n) (string-copy text (* n group-len) (min (* (1+ n) group-len) text-len)))
                        (iota (ceiling (/ text-len group-len)))))))

(define (extended-gcd a b)
  (let ((old-r a)
        (old-s 1)
        (s 0)
        (old-t 0)
        (t 1))
    (do ((r b)) ((zero? r))
      (let ((q (quotient old-r r)))
        ;; (par-set! '(old-r . r) r (- (* q r) old-r))
        (define temp r)
        (set! r (- old-r (* q r)))
        (set! old-r temp)
        ;; (par-set! '(old-s . s) s (- (* q s) old-s))
        (set! temp s)
        (set! s (- old-s (* q s)))
        (set! old-s temp)
        ;; (par-set! '(old-t . t) t (- (* q t) old-t))))
        (set! temp t)
        (set! t (- old-t (* q t)))
        (set! old-t temp)))

    ;; old-s ~ s_k and old-t ~ t_k : Bezout coefficients -- gcd(a, b) = r_k = a * s_k + b * t_k
    ;; the quotients of a and b by their gcd are s and t
    ;; old-r ~ r_k : the gcd
    (values old-s old-t old-r s t)))

(define (multiplicative-modular-inverse a b)
  (define-values (mmi-a-b _tk gcd-a-b _sk+1 _tk+1 ) (extended-gcd a b))
  (values (modulo mmi-a-b b) gcd-a-b))
