;;; matching-brackets.el --- Matching Brackets (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;; This version employs dolist instead of recursion.
;; I continue to be pretty sure there should be a built-in for
;; filter, at least.

;;; Code:


(defun is-paired (value)
  "Check if VALUE has properly matched round, square and curly parens."
  (pair-helper-p
   (filter-dolist (lambda (x) (member x '("(" "[" "{" ")" "]" "}")))
                  (split-string value "" 't))))


(defun pair-helper-p (pars)
  "Check if PARS, a list of parentheses chars, are correctly matched."
  (let ((stack))
    (catch 'mismatch
      (dolist (elem pars (null stack))
        (message "elem: %s stack: %s" elem stack)
        (if (member elem '("(" "[" "{"))
            (setq stack (cons elem stack))
          (if (not (paired-parens-p (car stack) elem))
              (throw 'mismatch nil)
            (setq stack (cdr stack))))))))


(defun paired-parens-p (par-left par-right)
  "Check if PAR-LEFT and PAR-RIGHT are matching parens."
  (member (pair par-left par-right) '(("(" ")") ("[" "]") ("{" "}"))))


(defun pair (a b)
  "Construct a pair (a list) out of A and B."
  (cons a (cons b nil)))


(defun filter-dolist (pred list)
  "Filter LIST using PRED."
  (let (result)
    (dolist (element (reverse list) result)
      (if (apply pred element ())
          (setq result (cons element result))))))


(provide 'matching-brackets)
;;; matching-brackets.el ends here
