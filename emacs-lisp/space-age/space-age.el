;;; space-age.el --- Space Age (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun age (planet seconds)
  "SECONDS as age on PLANET."
   (/ seconds space-age--earth-year (alist-get planet space-age--relative-orbits)))

(defconst space-age--earth-year 31557600 "Earth year in seconds.")
(defconst space-age--relative-orbits
  '((:earth . 1.0) (:mercury . 0.2408467) (:venus . 0.61519726)
    (:mars . 1.8808158) (:jupiter . 11.862615) (:saturn . 29.447498)
    (:uranus . 84.016846) (:neptune . 164.79132))
  "Relative orbit durations for various plantes.")

(provide 'space-age)
;;; space-age.el ends here

