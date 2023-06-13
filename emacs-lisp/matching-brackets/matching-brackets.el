;;; matching-brackets.el --- Matching Brackets (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defconst matching-brackets--parentheses-pairs
  '(("(" ")") ("[" "]") ("{" "}"))
  "Pairs of matching parentheses.")

(defconst matching-brackets--all-parentheses
  (flatten-list matching-brackets--parentheses-pairs)
  "All parentheses characters.")

(defconst matching-brackets--opening-parentheses
  (mapcar #'car matching-brackets--parentheses-pairs)
  "Character symbols of opening (left) parentheses.")


(defun is-paired (value)
  "Check if VALUE has properly matched round, square and curly parens."
  (matching-brackets--pair-helper-p
   (seq-filter
    (lambda (x) (member x matching-brackets--all-parentheses))
    (split-string value "" 't))))


(defun matching-brackets--pair-helper-p (pars)
  "Check if PARS, a list of parentheses chars, are correctly matched."
  (let ((stack))
    (catch 'mismatch
      (dolist (elem pars (null stack))
        (if (member elem matching-brackets--opening-parentheses)
            (push elem stack)
          (unless (matching-brackets--paired-parens-p (car stack) elem)
            (throw 'mismatch nil))
          (pop stack))))))


(defun matching-brackets--paired-parens-p (par-left par-right)
  "Check if PAR-LEFT and PAR-RIGHT are matching parens."
  (member (list par-left par-right) matching-brackets--parentheses-pairs))


(provide 'matching-brackets)
;;; matching-brackets.el ends here
