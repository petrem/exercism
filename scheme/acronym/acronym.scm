(import (rnrs))

(define (acronym test)
  (define (make-detect-transition-stm on-word-start)
    (let* ((char-set:word-to-sep (list->char-set '(#\space #\-)))
           (char-set:sep-to-word char-set:letter)
           (cs-for-state `((search-for-sep . ,char-set:word-to-sep)
                           (search-for-word-start . ,char-set:sep-to-word)))
           (state 'search-for-word-start))
      (lambda (ch)
        (when (char-set-contains? (assq-ref cs-for-state state) ch)
          (cond ((eq? state 'search-for-word-start)
                 (on-word-start ch)
                 (set! state 'search-for-sep))
                (else
                 (set! state 'search-for-word-start)))))))
  (let* ((result '())
         (emit (lambda (ch) (set! result (cons ch result))))
         (stm-input (make-detect-transition-stm emit)))
    (string-for-each stm-input test)
    (string-upcase! (list->string (reverse result)))))
          
      
