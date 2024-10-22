;;; bank-account.el --- Bank Account (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(define-error 'account-closed
              "Account is closed.")
(define-error 'account-open
              "Account is open.")
(define-error 'account-overdraw
              "Overdraw.")
(define-error 'account-negative-transaction
              "Negative transaction.")

(require 'eieio)

(defclass bank-account () ;; (operations)
  ((balance :initform 0
             :type number
             :protection :protected
             :documentation "Account balance.")
   (closed :initform t
;;           :type list
           :protection :protected
           :documentation "Non-nil when the balance is closed."))
  "A class implementing a bank account.")

(cl-defmethod balance ((acct bank-account))
  "Return the account balance from ACCT."
  (check-open acct)
  (slot-value acct 'balance))

(cl-defmethod open-account ((acct bank-account))
  "Open an account ACCT."
  (if (null (slot-value acct 'closed))
      (signal 'account-open nil)
    (setf (slot-value acct 'closed) nil)
    (setf (slot-value acct 'balance) 0))
  )

(cl-defmethod close-account ((acct bank-account))
  "Close an account ACCT."
  (check-open acct)
  (setf (slot-value acct 'closed) t)
  )

(cl-defmethod deposit ((acct bank-account) amount)
  "Deposit AMOUNT into account ACCT."
  (check-open acct)
  (when (< amount 0)
    (signal 'account-negative-transaction nil))
  (setf (slot-value acct 'balance) (+ (slot-value acct 'balance) amount))
  )

(cl-defmethod withdraw ((acct bank-account) amount)
  "Withdraw AMOUNT from account ACCT."
  (check-open acct)
  (when (< amount 0)
    (signal 'account-negative-transaction nil))
  (let ((current (slot-value acct 'balance)))
    (when (< current amount)
      (signal 'account-overdraw nil))
    (setf (slot-value acct 'balance) (- current amount)))
  )

(cl-defmethod check-open ((acct bank-account))
  (unless (null (slot-value acct 'closed))
    (signal 'account-closed nil)))

(defun make-new-bank-account () (bank-account))

(provide 'bank-account)
;;; bank-account.el ends here
