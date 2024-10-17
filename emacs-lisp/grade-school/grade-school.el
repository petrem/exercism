;;; grade-school.el --- Grade School (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun make-school () '())

(defun roster (school)
  (flatten-list
   (mapcar (lambda (elt) (sort (cdr elt) #'string-lessp))
           (sort school (lambda (a b) (< (car a) (car b)))))))

(defmacro consf (place value)
  (gv-letplace (getter setter) place
    (funcall setter `(cons ,value ,getter))))

(defmacro add (school name grade)
  `(unless (member ,name (roster ,school))
    (consf (alist-get ,grade ,school) ,name)))

(defun grade (school grade)
  (sort (alist-get grade school) #'string-lessp))

(provide 'grade-school)
;;; grade-school.el ends here
