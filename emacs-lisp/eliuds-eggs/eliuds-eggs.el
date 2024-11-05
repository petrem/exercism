;;; eliuds-eggs.el --- Eliud's Eggs (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun egg-count (number &optional count)
  (unless count (setq count 0))
  (if (> number 0)
      (egg-count (ash number -1) (+ count (logand number 1)))
    count))


(provide 'eliuds-eggs)
;;; eliuds-eggs.el ends here

