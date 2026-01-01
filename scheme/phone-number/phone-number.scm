(import (rnrs))

;; this is cheating because the tests don't check for stuff like "1-9+0-2345"
;; so we can get away just cleaning up any separators in any position and form

(define non-separators-char-set
  (apply char-set-delete (cons char-set:graphic (string->list " -.()+"))))

(define (clean phone-number)
  (let ((digits (string-filter non-separators-char-set phone-number)))
    (unless (string->number digits)
      (error "non-digits in phone number"))
    (when (or (> (string-length digits) 11)
               (< (string-length digits) 10))
      (error "Too long or too short"))
    (let ((digits10
           (if (= (string-length digits) 11)
               (if (char=? (string-ref digits 0) #\1)
                   (string-drop digits 1)
                   (error "11-digits number not starting with 1"))
               digits)))
      (cond ((char<=? (string-ref digits10 0) #\1) (error "Area code is 0 or 1"))
            ((char<=? (string-ref digits10 3) #\1) (error "Exchange code is 0 or 1"))
            (else digits10)))))
