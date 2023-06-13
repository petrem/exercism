;;; robot-name.el --- Robot Name (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Build a robot with a name like AA000, that can be reset
;; to a new name. Instructions are in README.md
;;

;;; Code:

(defun build-robot ()
  "Build a new robot with a random name."
  (let ((robot (cons nil ())))
    (reset-robot robot)
    robot))


(defun robot-name (robot)
  "Get the ROBOT's name."
  (car robot))


(defvar robot-name--allocated-names nil
  "Accumulate names allocated to robots to avoid duplicates.")


(defun reset-robot (robot)
  "Reset the name of ROBOT.  Factory reset!"
  (let (name)
    (while
        (progn
          (setq name (robot-name--make-name))
          (member name robot-name--allocated-names)))
    (push name robot-name--allocated-names)
    (setcar robot name)))


(defun robot-name--make-name ()
  "Generate a robot name, randomly."
  (let ((patterns '((?A 26) (?A 26) (?0 10) (?0 10) (?0 10))))
    (concat
     (mapcar
      (lambda (pattern) (+ (car pattern) (random (cadr pattern))))
      patterns))))


(provide 'robot-name)
;;; robot-name.el ends here
