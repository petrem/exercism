;;; run-length-encoding.el --- run-length-encoding Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun run-length-encode (s)
  (defun encode-group (groupping)
    (let ((len (length groupping)))
      (if (= len 1)
          groupping
        (format "%d%c" len (seq-first groupping)))))
  (mapconcat #'encode-group (group s)))

(defun run-length-decode (s &optional accum)
  (unless accum (setq accum ""))
  (if (string-empty-p s)
      accum
    (pcase-let* ((`(,digits ,rest) (span s (lambda (c) (<= ?0 c ?9))))
                (repeat (if (seq-empty-p digits) 1 (string-to-number digits)))
                (current (seq-first rest))
                (rest (seq-rest rest)))
      (run-length-decode rest (concat accum (make-string repeat current))))))

(defun span (sequence pred)
  (if (seq-empty-p sequence)
      `(,sequence ,sequence)
    (let ((matches (seq-take-while pred sequence)))
      `(,matches ,(seq-drop sequence (length matches))))))

(defun group (sequence)
  (defun go (seq groups)
    (if (seq-empty-p seq)
        groups
      (pcase-let ((`(,g ,rest) (span seq (lambda (c) (equal c (seq-first seq))))))
        (go rest (cons g groups)))))
  (seq-reverse (go sequence nil)))

(provide 'run-length-encoding)
;;; run-length-encoding.el ends here
