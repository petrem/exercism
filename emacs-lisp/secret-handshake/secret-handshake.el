;;; secret-handshake.el --- Secret Handshake (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun commands (number)
  (let ((reverse-requested nil) (result))
    (dolist (action-pair
             '((1 . "wink") (2 . "double blink") (4 . "close your eyes") (8 . "jump") (16 . reverse)))
      (pcase-let ((`(,k . ,action) action-pair))
        (when (/= 0 (logand number k))
          (if (stringp action) (push action result) (setq reverse-requested t)))))
    (if reverse-requested result (reverse result))))


(provide 'secret-handshake)
;;; secret-handshake.el ends here

