;;; scratch.el -- Summary

;;; Commentary:

;;; Code:

(setq animals '(gazelle giraffe lion tiger))

(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (print (car list))
    (setq list (cdr list))))

(print-elements-of-list animals)

(apply '+ 1 2 (+ 10 11))

(defun foldr (operator list initial)
  (if list
      (apply operator (car list) (foldr operator (cdr list) initial) ())
    initial
    ))

(foldr '+ '(1 2 3) 0)

(defun matched-helper-p (pars par-stack)
  (message "pars: %s stack: %s" pars par-stack)
  (if (null pars)
      (null par-stack)
    (if (member (car pars) '("(" "[" "{"))
        (matched-helper-p (cdr pars) (cons (car pars) par-stack))
      (and (closes-p (car par-stack) (car pars))
           (matched-helper-p (cdr pars) (cdr par-stack))))))

(matched-p "ab[]")
(matched-p "ab[(cd)(])")

(defun closes-p (par-left par-right)
  (member (pair par-left par-right) '(("(" ")") ("[" "]") ("{" "}"))))

(closes-p "[" "]")

(defun pair (a b) (cons a (cons b nil)))

(defun filter-dolist (pred list)
  (let (result)
    (dolist (element (reverse list) result)
      (if (apply pred element ())
          (setq result (cons element result))))))

(defun filter-recursive (pred list)
  (if list
      (if (apply pred (car list) ())
          (cons (car list) (filter-recursive pred (cdr list)))
        (filter-recursive pred (cdr list)))
    nil))

(filter-recursive (lambda (x) (if (> x 0) 't nil)) '(1 0 3 -1))

(defun matched-p (expr)
  (matched-helper-p
   (filter-dolist (lambda (x) (member x '("(" "[" "{" ")" "]" "}")))
                  (split-string expr "" 't))
   nil))


(let ((a) (b))
  (cons 'x b))

(provide 'scratch)
;;; scratch.el ends here
