;;; pig-latin.el --- Pig Latin (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'subr-x)

(defun translate (phrase)
  (string-join (mapcar #'piglify-word (string-split phrase "[[:blank:]]+")) " "))

(defun piglify-word (word)
  (if (seq-some (lambda (vowel) (string-prefix-p vowel word)) '("a" "e" "i" "o" "u" "xr" "yt"))
      (concat word "ay")
    (let* ((first-vowel-index (seq-position (seq-drop word 1) '(?a ?e ?i ?o ?u ?y) (lambda (chr vowels) (seq-contains-p vowels chr))))
           (split-at (if (null first-vowel-index) (length word) (1+ first-vowel-index)))
           (prefix (seq-take word split-at))
           (suffix (seq-drop word split-at)))
      (if (and (= ?u (seq-first suffix))
               (= ?q (seq-first (reverse prefix))))
          (concat (string-remove-prefix "u" suffix) (string-remove-suffix "q" prefix) "quay")
        (concat suffix prefix "ay")))))

(provide 'pig-latin)
;;; pig-latin.el ends here

