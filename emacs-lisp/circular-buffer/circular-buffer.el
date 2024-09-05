;;; circular-buffer.el --- Circular Buffer (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(define-error 'empty-buffer-error
              "The buffer is empty.")

(define-error 'full-buffer-error
              "The buffer is full.")

(defclass circular-buffer ()
  ((capacity :initarg :capacity
             :type number
             :documentation "How many slots can the buffer hold.")
   (size :initform 0
         :type number
         :protection :protected
         :documentation "Current size of the buffer.")
   (ring :type list
         :protection :protected
         :documentation "Internal structure holding the buffer data.")
   (reader :type list
           :protection :protected)
   (writer :type list
           :protection :protected))
  "A class implementing a ring buffer.")


(cl-defmethod maybe-initialize-ring ((buf circular-buffer))
  "Initialize the ring structure of BUF, if necessary."
  (if (not (slot-boundp buf 'ring))
      (prog1 nil
        (setf (slot-value buf 'ring) (make-list (slot-value buf 'capacity) nil))
        (setf (nthcdr (slot-value buf 'capacity) (slot-value buf 'ring))
              (slot-value buf 'ring))
        (setf (slot-value buf 'size) 0)
        (setf (slot-value buf 'reader) (slot-value buf 'ring))
        (setf (slot-value buf 'writer) (slot-value buf 'ring)))))


(cl-defmethod clear ((buf circular-buffer))
  "Reset BUF to empty state."
  (slot-makeunbound buf 'ring)
  (maybe-initialize-ring buf))
  

(cl-defmethod write ((buf circular-buffer) value)
  "Push VALUE into BUF."
  (if (= (slot-value buf 'capacity) (slot-value buf 'size))
      (signal 'full-buffer-error nil))
  (maybe-initialize-ring buf)
  (prog1 nil
    (setf (slot-value buf 'size) (1+ (slot-value buf 'size)))
    (setf (car (slot-value buf 'writer)) value)
    (setf (slot-value buf 'writer) (cdr (slot-value buf 'writer)))))


(cl-defmethod read-buff ((buf circular-buffer))
  "Pop a value from BUF."
  (if (= (slot-value buf 'size) 0)
      (signal 'empty-buffer-error nil))
  (maybe-initialize-ring buf)
  (prog2
      (setf (slot-value buf 'size) (1- (slot-value buf 'size)))
      (car (slot-value buf 'reader))
    (setf (slot-value buf 'reader) (cdr (slot-value buf 'reader)))))


(cl-defmethod overwrite ((buf circular-buffer) value)
  "Overwrite last slot in the ring of BUF with VALUE."
  (maybe-initialize-ring buf)
  (prog1 nil
    (setf (car (slot-value buf 'writer)) value)
    (setf (slot-value buf 'writer) (cdr (slot-value buf 'writer)))
    (if (= (slot-value buf 'capacity) (slot-value buf 'size))
        (setf (slot-value buf 'reader) (cdr (slot-value buf 'reader)))
      (setf (slot-value buf 'size) (1+ (slot-value buf 'size))))))

(provide 'circular-buffer)
;;; circular-buffer.el ends here

