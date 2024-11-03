;;; minesweeper.el --- Minesweeper (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;; This code is more complicated than it needs to be, in particular due to the approach
;; I've taken. Even for the approach, I expect the code itself could be improved, but
;; I'm an (E)Lisp noob.

;; For each mine, I generate a "field of 1s" in the vicinity of the mine, where it
;; "influences" the mine sensors around it.

;; Assuming the mine is on row 2, second position, this will look like
;; ((1 [1 1 1]) (2 [1 NaN 1]) (3 [1 1 1])).

;; These are clamped down accordingly when the mine is near a side or corner.

;; Then we add up all the fields row by row.

;; Great fun!

;;; Code:


(defun annotate (minefield)
  "Annotate MINEFIELD with number of adjacent mines."
  (let ((mine   0.0e+NaN)
        (nrows  (length minefield))
        (rowlen (length (car minefield))))
    
    (defun clamp-values (col-index values)
      (let* ((drop (if (= col-index 0) 1 0))
             (take (if (> (1- rowlen) col-index) 3 2)))
        (seq-into (seq-drop (seq-take values take) drop) 'vector)))
    
    (defun mk-influence-row (row-index col-index values)
      (when (<= 0 row-index (1- nrows))
        (list row-index
              (vconcat (make-vector (max (1- col-index) 0) 0)
                       (clamp-values col-index values)
                       (make-vector (max (- rowlen col-index 2) 0) 0)))))
    
    (defun addup (group)
      (apply #'seq-mapn #'+ (mapcan #'cdr group)))

    (defun to-char (num)
      (cond
        ((= num 0) ? )
        ((and (floatp num) (isnan num)) ?*)
        (t (+ ?0 num))))

    (defun to-chars (numbers)
      (mapcar (lambda (row) (apply #'string (mapcar #'to-char row))) numbers))
    
    (let* ((ungrouped
            (apply #'append
                   (seq-map-indexed
                    (lambda (row row-index)
                      (apply #'append
                             (seq-map-indexed
                              (lambda (cell cell-index)
                                (when (equal cell ?*)
                                  (delq nil
                                        `(,(mk-influence-row (1- row-index) cell-index '(1   1   1))
                                          ,(mk-influence-row row-index      cell-index `(1 ,mine 1))
                                          ,(mk-influence-row (1+ row-index) cell-index '(1   1   1))))))
                              row)))
                    minefield)))
           (grouped (seq-group-by #'car ungrouped)))

      (to-chars
       (seq-map (lambda (row-index)
                  (addup
                   (alist-get row-index grouped `((,row-index ,(make-vector rowlen 0))))))
               (number-sequence 0 (1- nrows)))))))

(provide 'minesweeper)
;;; minesweeper.el ends here
