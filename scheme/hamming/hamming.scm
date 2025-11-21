(import (rnrs))

(define (hamming-distance strand-a strand-b)
  ;; is there a zip procedure? :-|
  (cond
   ((not (= (string-length strand-a) (string-length strand-b)))
    (error "strings should be of equal length")) ;; and I should not iterate them all the time, assuming string-length must do that (does it?) -- but even if not...
   ((string= strand-a "")
    0)
   ((string=
     (string-take strand-a 1)
     (string-take strand-b 1))
    (hamming-distance (string-drop strand-a 1) (string-drop strand-b 1)))
   (#t
    (1+ (hamming-distance (string-drop strand-a 1) (string-drop strand-b 1))))))

