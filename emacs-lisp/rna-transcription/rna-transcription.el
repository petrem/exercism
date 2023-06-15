;;; rna-transcription.el -- RNA Transcription (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun to-rna (strand)
  "Translate ADN nucleotides in STRAND to RNA."
  (concat
   (mapcar
    (lambda (n) (cdr (assoc n '((?G . ?C) (?C . ?G) (?T . ?A) (?A . ?U)))))
    strand)))

(provide 'rna-transcription)
;;; rna-transcription.el ends here
