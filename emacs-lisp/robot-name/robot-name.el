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
    robot)
)

(defun robot-name (robot)
  "Get the ROBOT's name."
  (car robot)
)

(defun reset-robot (robot)
  "Reset the name of ROBOT.  Factory reset!"
  (setcar robot (make-robot-name))
)

(setq robot-name-generated-names nil)

(defun make-robot-name ()
  "Generate a robot name, randomly."
  (let ((patterns '((?A 26) (?A 26) (?0 10) (?0 10) (?0 10)))
        name)
    (while (progn
             (let (result)
               (dolist (pattern (reverse patterns) result)
                 (setq result (cons (+ (car pattern) (random (cadr pattern))) result)))
               (setq name (concat result)))
             (member name robot-name-generated-names)))
    (setq robot-name-generated-names (cons name robot-name-generated-names))
    name)
)

(provide 'robot-name)
;;; robot-name.el ends here
