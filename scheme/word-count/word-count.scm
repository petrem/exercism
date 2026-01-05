(import (rnrs))

(define (word-count sentence)
  (define (increment-word-count word counts)
    (let ((current-count (assoc-ref counts word)))
      (assoc-set! counts word (1+ (or current-count 0)))))

  (define (split-words sentence)
    (let* ((token-set (char-set-adjoin char-set:letter+digit #\'))
           (words-with-apostrophes (string-tokenize sentence token-set)))
      (map (lambda (word)
             (if (and (string-prefix? "'" word)
                      (string-suffix? "'" word))
                 (string-trim-both word #\')
                 word))
           words-with-apostrophes)))
           

  (let loop ((words (split-words (string-downcase sentence)))
             (counts '()))
    (if (null? words)
        counts
        (loop (cdr words) (increment-word-count (car words) counts)))))

