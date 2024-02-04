;;; raindrops.el --- Raindrops (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defconst raindrops--rainspeak '([3 "Pling"] [5 "Plang"] [7 "Plong"]))

(defsubst raindrops--string-join (strings)
  "Join all STRINGS."
  (mapconcat #'identity strings nil))

(defun convert (n)
  "Convert integer N to its raindrops string."
  (let* ((as-rain
          (seq-reduce
           (lambda (acc num-word)
             (seq-let [num word] num-word
               (cons (if (eq 0 (% n num)) word) acc)))
           raindrops--rainspeak
           () ))
         (as-rain-str (raindrops--string-join (reverse as-rain))))
    (if (seq-empty-p as-rain-str)
        (number-to-string n)
      as-rain-str))
)

(provide 'raindrops)
;;; raindrops.el ends here
