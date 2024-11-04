;;; dnd-character.el --- D&amp;D Character (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun modifier (score)
  (floor (/ (- score 10.0) 2)))


(defun ability ()
  (defsubst throw-dice () (1+ (random 7)))
  (apply #'+ (cdr (sort (list (throw-dice) (throw-dice) (throw-dice) (throw-dice)) #'>))))


(defun generate-dnd-character ()
  (record 'dnd-character (ability) (ability) (ability) (ability) (ability) (ability) (ability)))


(provide 'dnd-character)
;;; dnd-character.el ends here

