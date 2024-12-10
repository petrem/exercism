;;; largest-series-product.el --- Largest Series Product (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun largest-product (digits span)
  (cond 
   ((< span 0) (error "Span is negative"))
   ((and (string-empty-p digits) (/= span 0)) (error "span is not zero but string is empty"))
   ;; could be performed while transforming to numeric, but this way
   ;; error checking is separated and its unlikely to be a big performance hit
   ((seq-some (lambda (d) (not (<= ?0 d ?9))) digits) (error "contains non-digit characters"))
   ((> span (length digits)) (error "span longer than string length")))
  (let ((max-prod 0))
    (mapc
     (lambda (from)
       (let ((prod (digits-product (substring digits from (+ from span)))))
         (when (> prod max-prod)
           (setq max-prod prod))))
     (number-sequence 0 (- (length digits) span)))
    max-prod))

(defun digits-product (digits)
  (apply #'* (mapcar (lambda (d) (- d ?0)) digits)))

(provide 'largest-series-product)
;;; largest-series-product.el ends here
