;;; dnd-character.el --- D&amp;D Character (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; This macro is taken from https://exercism.org/tracks/emacs-lisp/exercises/dnd-character/solutions/leetwinski
(defmacro rep (n form) `(cl-loop repeat ,n collect ,form))


(defun modifier (score)
  (floor (/ (- score 10.0) 2)))


(defun ability ()
  (apply #'+ (cdr (sort (rep 4 (1+ (random 6))) #'<))))


(defun generate-dnd-character ()
  (let* ((attrs (rep 6 (ability)))
         (hitpoints (+ 10 (modifier (nth 3 attrs)))))
  (apply #'record 'dnd-character `(,@attrs ,hitpoints))))


(provide 'dnd-character)
;;; dnd-character.el ends here

