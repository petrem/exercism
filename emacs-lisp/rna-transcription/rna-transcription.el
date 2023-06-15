;;; rna-transcription.el -- RNA Transcription (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun rna-transcript--translate-nucleotide (nucleotide)
  "Transform one dna NUCLEOTIDE (which must be a character) to its rna counterpart."
  (defconst dna-to-rna-map  '((?G . ?C) (?C . ?G) (?T . ?A) (?A . ?U)))
  (or
   (alist-get nucleotide dna-to-rna-map nil nil '=)
   (error (format "Unknown nucleotide: %s" nucleotide))))
  

(defun to-rna (strand)
  "Translate ADN nucleotides in STRAND to RNA."
  (concat (mapcar #'rna-transcript--translate-nucleotide strand)))


(provide 'rna-transcription)
;;; rna-transcription.el ends here
