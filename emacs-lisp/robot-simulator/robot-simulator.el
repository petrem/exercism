;;; robot-simulator.el --- robot-simulator (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun create-robot (x y direction)
  (record 'robot x y direction))

(defun move (robot instructions)
  (seq-reduce #'move-robot instructions robot))

(defun move-robot (robot instruction)
  (let ((x (aref robot 1))
        (y (aref robot 2))
        (dir (aref robot 3)))
    (pcase instruction
      (?L (create-robot x y (rotate-direction dir -1)))
      (?R (create-robot x y (rotate-direction dir 1)))
      (?A (let ((delta (alist-get dir bearings)))
             (create-robot (+ x (aref delta 1)) (+ y (aref delta 2)) dir))))))
    
(defconst bearings
  '((north . [0 0 1]) (east . [1 1 0]) (south . [2 0 -1]) (west . [3 -1 0])))

(defsubst rotate-direction (direction by)
  (car (nth (mod (+ (aref (alist-get direction bearings) 0) by) 4) bearings)))

(provide 'robot-simulator)
;;; robot-simulator.el ends here
