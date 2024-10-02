;;; hamming.el --- Hamming (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun hamming-distance (dna1 dna2)
  (if (= (length dna1) (length dna2))
      (seq-count #'null (string-zip-with (lambda (a b) (equal a b)) dna1 dna2))
    (error "Strands of different length, begone!")))

;; note there's 'seq-mapn' for such things

(defun string-zip-with (fun str1 str2)
  (let ((zipped))
    (while (not (or (string-empty-p str1) (string-empty-p str2)))
      (setq zipped (cons
                    (funcall fun (seq-first str1) (seq-first str2))
                    zipped)
            str1 (seq-drop str1 1)
            str2 (seq-drop str2 1)))
    zipped))

(provide 'hamming)
;;; hamming.el ends here
