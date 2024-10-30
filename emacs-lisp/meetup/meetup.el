;;; meetup.el --- Meetup (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'calendar)



(defun meetup (year month dayofweek schedule)
  (let* ((dayname (alist-get dayofweek day-sym-to-number))
         (nth-day (alist-get schedule nth-days)))
    (pcase (calendar-nth-named-day nth-day dayname month year (when (eq schedule :teenth) 13))
      (`(,month ,day ,year) (list year month day)))))

(defconst day-sym-to-number '((:sunday . 0) (:monday . 1) (:tuesday . 2) (:wednesday . 3) (:thursday . 4) (:friday . 5) (:saturday . 6)))

(defconst nth-days '((:first . 1) (:second . 2) (:third . 3) (:fourth . 4) (:fifth . 5) (:last . -1) (:teenth . 1)))

(provide 'meetup)
;;; meetup.el ends here

