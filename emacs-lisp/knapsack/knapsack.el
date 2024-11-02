;;; knapsack.el --- Knapsack (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun maximum-value (maximum-weight items)
  (let ((useful-items (seq-filter (lambda (item) (<= (alist-get :weight item) maximum-weight)) items))
        (max-value 0))
    (while-let ((item (pop useful-items))
                (current (+ (alist-get :value item)
                            (maximum-value (- maximum-weight (alist-get :weight item)) (seq-copy useful-items)))))
      (setq max-value (if (< max-value current) current max-value)))
    max-value))


(provide 'knapsack)
;;; knapsack.el ends here

