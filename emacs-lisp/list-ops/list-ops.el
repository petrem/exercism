;;; list-ops.el --- List Ops (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;; For diversity, I'll be implement some functions with recursion, some with looping
;; constructs, some based on the fold implementations etc.

;;; Code:

;; Let's do foldr recursively

(defun list-foldr (fun list accu)
  "Fold (reduce) each item in LIST into the ACCU using FUN, from the right."
  (if (null list)
      accu
    (funcall fun (car list) (list-foldr fun (cdr list) accu))))

;; Now let's have some developer's fun and define foldl in terms of foldr!
;; Instead of:
;;
;; (defun list-foldl (fun list accu)
;;   "Fold (reduce) each item in LIST into the ACCU using FUN, from the left."
;;   (if (null list)
;;       accu
;;     (list-foldl fun (cdr list) (funcall fun accu (car list)))))
;;
;; let's make this "interesting" (read: really cumbersome)

(defun list-foldl (fun list accu)
  "Fold (reduce) each item in LIST into the ACCU using FUN, from the left."
  (funcall
   (list-foldr (lambda (x g) (list-ops--list-foldl-step fun x g)) list #'identity) accu))

(defun list-ops--list-foldl-step (f x g)
  "Helper getting the foldl's F, an element X, and the closure G created so far."
  (lambda (a) (funcall g (funcall f a x))))

;; Let's use folds for a few of these...

;; I wonder if I could do this to shortcut on the first element. Perhaps something like
;; (and (null (car l)) (null (cdr l))), trouble being that this wouldn't work on
;; '(nil) and similar.
;; I don't think this is of any use: (list-foldl (lambda (a _) (and a nil)) list t))
(defun list-empty-p (list)
  "Return whether LIST is empty."
  (list-foldl (lambda (_ _) nil) list t))

(defun list-sum (list)
  "Sum all the elements in LIST."
  (list-foldl #'+ list 0))

;; Could be (apply #'+ (mapcar (lambda (_) 1) list)) but at least reduce is more clear.
(defun list-length (list)
  "Total number of elements in LIST."
  (seq-reduce (lambda (len _) (1+ len)) list 0))

;; This is not quite what the exercise requires, but tests use non-mutable lists.
;; I'll use the \"classic\" `dolist' macro for this one.
(defun list-append (list1 list2)
  "Return a new list with all items in listp LIST2 after those in listp LIST1."
  (let ((newlist (copy-sequence list2)))
    (dolist (elt (list-reverse list1) newlist)
      (setq newlist (cons elt newlist)))))

;; Given the `(elisp)Iteration' info page has exactly reversing a list as an
;; example, I'll also do this one recursively.
(defun list-reverse (list)
  "Return a new list with elements of LIST reversed."
  (list-ops--list-reverse-loop list nil))

(defun list-ops--list-reverse-loop (forward-list reversed-list)
  "Add elements in FORWARD-LIST to REVERSED-LIST in reversed order.

This is a helper for `list-reverse'."
  (if (null forward-list)
      reversed-list
    (list-ops--list-reverse-loop (cdr forward-list) (cons (car forward-list) reversed-list))))

;; Evidently, avoid using `append', `nconc' or similar, as required by the exercise.
(defun list-concatenate (list1 list2 &rest lists)
  "Combine all items in LIST1, LIST2 and any other LISTS into one flattened list."

  (let ((combined (cons list1 (cons list2 lists)))
        result)
    (dolist (sublist combined (list-reverse result))
      (dolist (elt sublist)
        (setq result (cons elt result))))))


(defun list-filter (list predicate)
  "Return the list of all items in LIST for which (PREDICATE item) is true."
  (reverse
   (seq-reduce (lambda (acc x)
                 (if (funcall predicate x) (cons x acc) acc)) list nil)))

;; 'mapcar' is surely faster...
(defun list-map (list fun)
  "Return a new list of the results of applying FUN to the elements of LIST."
  (let (result)
    (dolist (x list (reverse result))
      (push (funcall fun x) result))))

(provide 'list-ops)
;;; list-ops.el ends here
