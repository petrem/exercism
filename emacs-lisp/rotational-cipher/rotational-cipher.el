;;; rotational-cipher.el --- Rotational Cipher (exercism)  -*- lexical-binding: nil; -*-

;;; Commentary:

;;; Code:


(defun rotate (text shift-key)
  (mapconcat #'rotchr text))

(defsubst rotchr (chr)
  (list (cond ((<= ?A chr ?Z) (+ ?A (% (+ chr shift-key -13) 26)))
              ((<= ?a chr ?z) (+ ?a (% (+ chr shift-key -19) 26)))
              (t chr))))
        

(provide 'rotational-cipher)
;;; rotational-cipher.el ends here
