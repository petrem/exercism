(import (rnrs))

(define (transpose matrix)
  (if (null? matrix)
      matrix
      (let ((n-rows (length matrix))
            (n-cols (longest-row matrix)))
        (apply zip
               (map (lambda (row) (space-augment row n-cols))
                    matrix)))))

(define (longest-row matrix)
  (apply max (map length matrix)))

(define (space-augment xs len)
  (append xs (make-list (- len (length xs)) 32)))

(define (zip l1 . ls)
  (let loop ((l1 l1) (ls ls) (acc '()))
    (if (null? l1)
        (reverse! acc)
        (loop (cdr l1)
              (map cdr ls)
              (cons (cons (car l1) (map car ls))
                    acc)))))
