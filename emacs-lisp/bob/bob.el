;;; bob.el --- Bob (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun response-for (phrase)
  (let* ((case-fold-search nil)
         (is-question (string-match-p "\\?[[:space:]]*\\'" phrase))
         (is-yell (string-match-p "\\`[^a-z]*?[A-Z][^a-z]*\\'" phrase)))
    (alist-get
     (cond
      ((string-match-p "\\`[[:space:]]*\\'" phrase) :silence)
      ((and is-question is-yell) :yellstion)
      (is-question :question)
      (is-yell :yell)
      (t :default))
     lackadaisical-answers)))

(defconst lackadaisical-answers
  '((:question . "Sure.") (:yell . "Whoa, chill out!")
    (:yellstion . "Calm down, I know what I'm doing!")
    (:silence . "Fine. Be that way!") (:default . "Whatever.")))

(provide 'bob)
;;; bob.el ends here
