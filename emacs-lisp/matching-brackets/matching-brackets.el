;;; matching-brackets.el --- Matching Brackets (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;; This version employs dolist instead of recursion.
;; I'm using the prefix minus (e.g. `-foo-bar `) to mark things "private" to the module,
;; but I'm almost certainly wrong to do so.

;;; Code:


(defun is-paired (value)
  "Check if VALUE has properly matched round, square and curly parens."
  (-pair-helper-p (seq-filter (lambda (x) (member x -all-parentheses))
                              (split-string value "" 't))))


(defun -pair-helper-p (pars)
  "Check if PARS, a list of parentheses chars, are correctly matched."
  (let ((stack))
    (catch 'mismatch
      (dolist (elem pars (null stack))
        (if (member elem -opening-parentheses)
            (push elem stack)
          (unless (-paired-parens-p (car stack) elem)
            (throw 'mismatch nil))
          (pop stack))))))


(defun -paired-parens-p (par-left par-right)
  "Check if PAR-LEFT and PAR-RIGHT are matching parens."
  (member (list par-left par-right) parentheses-pairs))


(defconst parentheses-pairs
  '(("(" ")") ("[" "]") ("{" "}"))
  "Pairs of matching parentheses.")

(defconst -all-parentheses
  (flatten-list parentheses-pairs)
  "All parentheses characters.")

(defconst -opening-parentheses
  (mapcar #'car parentheses-pairs)
  "Character symbols of opening (left) parentheses.")


(provide 'matching-brackets)
;;; matching-brackets.el ends here
