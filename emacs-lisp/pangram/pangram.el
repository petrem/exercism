;;; pangram.el --- Pangram (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun pangramp (phrase) "Is PHRASE a pangram?" (null (seq-difference "abcdefghijklmnopqrstuvwxyz" (downcase phrase))))


(provide 'pangram)
;;; pangram.el ends here
