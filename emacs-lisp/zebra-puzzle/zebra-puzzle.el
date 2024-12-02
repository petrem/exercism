;;; zebra-puzzle.el --- Zebra Puzzle (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'generator)

;; TODO: would be nice to cache the solution, once generated

(defun drinks-water ()
  (resident-who (lambda (house) (string-equal "Water" (drink-of-house house))) (get-a-solution)))


(defun owns-zebra ()
  (resident-who (lambda (house) (string-equal "Zebra" (pet-of-house house))) (get-a-solution)))


;; Generate solutions

(defun get-a-solution nil
  (let ((solution-gen (generate-solutions)))
    (iter-next solution-gen)))

(iter-defun generate-solutions nil
  
  (dolist (residents (permutations '("Englishman" "Spaniard" "Ukrainian" "Norwegian" "Japanese")))
    (when (rule10 residents)
      (dolist (colors (permutations '("Red" "Green" "Ivory" "Yellow" "Blue")))
        (when (and (rule6 colors)
                   (rule2 colors residents)
                   (rule15 colors residents))
          (dolist (drinks (permutations '("Coffee" "Tea" "Milk" "Juice" "Water")))
            (when (and (rule9 drinks)
                       (rule5 residents drinks)
                       (rule4 colors drinks))
              (dolist (hobbies (permutations '("Dancing" "Paint" "Reading" "Chess" "Football")))
                (when (and (rule8 colors hobbies)
                           (rule13 drinks hobbies)
                           (rule14 residents hobbies))
                  (dolist (pets (permutations '("Dog" "Snail" "Fox" "Horse" "Zebra")))
                    (when (and (rule3 residents pets)
                               (rule7 pets hobbies)
                               (rule11 pets hobbies)
                               (rule12 pets hobbies))
                      (iter-yield (seq-mapn (lambda (r c p d h) (list r c p d h)) residents colors pets drinks hobbies))))))))))))
  ;; return normally
  -1)
    

;; Rules

(defun rule2 (colors residents)
  (check-paired "Red" "Englishman" colors residents))

(defun rule3 (residents pets)
  (check-paired "Spaniard" "Dog" residents pets))

(defun rule4 (colors drinks)
  (check-paired "Green" "Coffee" colors drinks))

(defun rule5 (residents drinks)
  (check-paired "Ukrainian" "Tea" residents drinks))

(defun rule6 (colors)
  (let ((idx (seq-position colors "Green")))
    (when (> (or idx -1) 0)
      (string-equal "Ivory" (nth (1- idx) colors)))))

(defun rule7 (pets hobbies)
  (check-paired "Snail" "Dancing" pets hobbies))

(defun rule8 (colors hobbies)
  (check-paired "Yellow" "Paint" colors hobbies))

(defun rule9 (drinks)
  (string-equal "Milk" (caddr drinks)))

(defun rule10 (residents)
  (string-equal "Norwegian" (car residents)))

(defun rule11 (pets hobbies)
  (check-next "Fox" "Reading" pets hobbies))

(defun rule12 (pets hobbies)
  (check-next "Horse" "Paint" pets hobbies))

(defun rule13 (drinks hobbies)
  (check-paired "Juice" "Football" drinks hobbies))

(defun rule14 (residents hobbies)
  (check-paired "Japanese" "Chess" residents hobbies))

(defun rule15 (colors residents)
  (check-next "Blue" "Norwegian" colors residents))
   
;; Utility functions

(defun check-paired (a b a-list b-list)
  (member (list a b) (zip a-list b-list)))

(defun check-next (a b a-list b-list)
  (member (list a b) (append (zip a-list (cdr b-list)) (zip (cdr a-list) b-list))))

(defun zip (a-list b-list)
  "Zip elements of A-LIST and B-LIST into a list of two-element lists."
  (seq-mapn (lambda (a b) (list a b)) a-list b-list))

(defun permutations (list)
  "Permutations of LIST."
  (cond
   ((null list) '(nil))
   ((null (cdr list)) (list (seq-copy list)))
   (t (let (result)
        (dolist (elt list result)
          (let* ((sub-perms (permutations (remove elt list)))
                 (new-perms (mapcar (lambda (l) (cons elt l)) sub-perms)))
            (setq result (nconc new-perms result))))))))

(defun resident-who (pred houses)
  "Resident (first element) of the house matched by PRED in HOUSES."
  (car (seq-find pred houses)))

(defun drink-of-house (house)
  (nth 3 house))

(defun pet-of-house (house)
  (nth 2 house))
  
(provide 'zebra-puzzle)
;;; zebra-puzzle.el ends here

