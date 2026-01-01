(use-modules (srfi srfi-1)
             (srfi srfi-13)
             (ice-9 hash-table))

(define (display-env env)
  (display "Environment:\n")
  (hash-for-each (lambda (key value) (display (format #f "\t~s = ~s\n" key value))) env))

(define (forth program)
  ;; case-insensitive hash and assoc functions for "environment variables"
  (define (env-hash str size)
    (remainder (string-hash-ci str) size))
    
  (define (env-assoc str alist)
    (find (lambda (pair) (string-ci=? str (car pair))) alist))

  (let ((stack '()))
    (define (push item)
      (set! stack (cons item stack)))

    (define (pop1)
      (when (null? stack)
        (error "Empty stack while popping a value"))
      (let ((top (car stack)))
        (set! stack (cdr stack))
        top))

    (define (pop2)
      (when (or (null? stack)
                (null? (cdr stack)))
        (error "Not enough elements to pop two values"))
      (let ((fst (car stack))
            (snd (cadr stack)))
        (set! stack (cddr stack))
        `(,fst . ,snd)))

    (define (forth-dup)
      (let ((top (pop1)))
        (push top)
        (push top)))

    (define (forth-drop)
      (pop1))

    (define (forth-swap)
      (let ((top2 (pop2)))
        (push (car top2))
        (push (cdr top2))))

     (define (forth-over)
      (let ((top2 (pop2)))
        (push (cdr top2))
        (push (car top2))
        (push (cdr top2))))
    
    (define (forth+)
      (let ((top2 (pop2)))
        (push (+ (cdr top2) (car top2)))))

    (define (forth-)
      (let ((top2 (pop2)))
        (push (- (cdr top2) (car top2)))))
    
    (define (forth*)
      (let ((top2 (pop2)))
        (push (* (cdr top2) (car top2)))))
    
    (define (forth/)
      (let ((top2 (pop2)))
        (push (quotient (cdr top2) (car top2)))))
    
    (let ((env (alist->hashx-table
                env-hash
                env-assoc
                `(("+" . ,forth+)
                  ("-" . ,forth-)
                  ("*" . ,forth*)
                  ("/" . ,forth/)
                  ("dup" . ,forth-dup)
                  ("drop" . ,forth-drop)
                  ("swap" . ,forth-swap)
                  ("over" . ,forth-over)))))

      (define (env-lookup var)
        (let ((handle (hashx-get-handle env-hash env-assoc env var)))
          (unless handle
            (error (format #f "Word ~s not found in environment!" var)))
          (cdr handle)))

      (define (env-set! var val)
        (when (string->number var)
          (error "Cannot assign to number!"))
        (hashx-set! env-hash env-assoc env var val))
      
      (let eval-forth ((prog program))
        (display (format #f "eval-forth ~s (stack: ~s)\n" prog stack))
        (unless (null? prog)
          (let* ((block (car prog))
                 (tokens (string-tokenize block)))
            (if (and (string=? (car tokens) ":")
                     (string=? (last tokens) ";"))
                (begin
                  (display (format #f "Assignment ~s = ~s\n" (cadr tokens) (drop-right (cddr tokens) 1)))
                  (let ((expanded (map (lambda (token) (if (string->number token)
                                                      token
                                                      (let ((val (env-lookup token)))
                                                        (cond ((procedure? val) token)
                                                              (else val)))))
                                       (drop-right (cddr tokens) 1))))
                    (env-set! (cadr tokens) (string-join expanded)))
                  (display-env env))
                (let tokens-loop ((tokens tokens))
                  (unless (null? tokens)
                    (let ((token (car tokens)))
                      (cond
                       ((string->number token) (push (string->number token)))
                       (else (let ((var (env-lookup token)))
                               (if (procedure? var)
                                   (var)
                                   (eval-forth `(,var)))))))
                    (tokens-loop (cdr tokens))))))
          (eval-forth (cdr prog))))
      (display-env env)
      stack)))
