;;; matching-brackets.el --- Matching Brackets (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;; This version employs recursion a lot. I'm pretty sure there should be a built-in for
;; filter, at least.

;;; Code:


(defun is-paired (value)
  "Check if VALUE has properly matched round, square and curly parens."
  (pair-helper-p
   (filter-recursive (lambda (x) (member x '("(" "[" "{" ")" "]" "}")))
                  (split-string value "" 't))
   nil))


(defun pair-helper-p (pars par-stack)
  "Recursively check if PARS, a list of parens chars, are correctly matched.
PAR-STACK should be nil, initially."
  (if (null pars)
      (null par-stack)
    (if (member (car pars) '("(" "[" "{"))
        (pair-helper-p (cdr pars) (cons (car pars) par-stack))
      (and (paired-parens-p (car par-stack) (car pars))
           (pair-helper-p (cdr pars) (cdr par-stack))))))

(defun paired-parens-p (par-left par-right)
  "Check if PAR-LEFT and PAR-RIGHT are matching parens."
  (member (pair par-left par-right) '(("(" ")") ("[" "]") ("{" "}"))))


(defun pair (a b)
  "Construct a pair (a list) out of A and B."
  (cons a (cons b nil)))

(defun filter-recursive (pred list)
  "Filter LIST using PRED."
  (if list
      (if (apply pred (car list) ())
          (cons (car list) (filter-recursive pred (cdr list)))
        (filter-recursive pred (cdr list)))
    nil))

(provide 'matching-brackets)
;;; matching-brackets.el ends here
