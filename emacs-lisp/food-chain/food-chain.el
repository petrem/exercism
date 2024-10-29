;;; food-chain.el --- Food Chain (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun recite (start-verse end-verse)
  (cdr (mapcan #'make-stanza (number-sequence (1- start-verse) (1- end-verse)))))

(defconst all-animals
  [("fly"    . ("I don't know why she swallowed the fly. Perhaps she'll die." last))
   ("spider" . ("wriggled and jiggled and tickled inside her" qualified))
   ("bird"   . ("How absurd to swallow a bird!"))
   ("cat"    . ("Imagine that, to swallow a cat!"))
   ("dog"    . ("What a hog, to swallow a dog!"))
   ("goat"   . ("Just opened her throat and swallowed a goat!"))
   ("cow"    . ("I don't know how she swallowed a cow!"))
   ("horse"  . ("She's dead, of course!" last))]
)


(defsubst name (animal) (car animal))

(defsubst is-last (animal) (member 'last (cdr animal)))

(defsubst is-qualified (animal) (member 'qualified (cdr animal)))

(defsubst utter (animal)
  (if (is-qualified animal)
      (concat "It " (cadr animal) ".")
    (cadr animal)))

(defsubst qualify (animal)
  (when (is-qualified animal) (concat " that " (cadr animal))))

(defsubst justify (animal1 animal2)
  (concat "She swallowed the " (name animal1) " to catch the " (name animal2) (qualify animal2) "."))

(defsubst take-until (testp seq)
  (let ((not-test-p (lambda (x) (not (funcall testp x)))))
    (seq-concatenate 'list (seq-take-while not-test-p seq) (seq-take (seq-drop-while not-test-p seq) 1))))

(defun make-stanza (index)
  (let* ((animal (aref all-animals index))
         (animals (take-until #'is-last (reverse (seq-take all-animals (1+ index)))))
         (tuplets (seq-mapn #'justify animals (seq-rest animals))))
    (nconc
     (list ""
           (concat "I know an old lady who swallowed a " (name animal) ".")
           (utter animal))
     (unless (is-last animal)
       (nconc tuplets (list (utter (car (last animals)))))))))

(provide 'food-chain)
;;; food-chain.el ends here
