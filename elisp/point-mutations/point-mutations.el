;;; point-mutations.el --- Point Mutations (exercism)

;;; Commentary:

;;; Code:

(require 'cl)

(defun length-equal-p (seq1 seq2)
  (= (length seq1) (length seq2)))

(defun string-to-list (str)
  (append str nil))g

(defun hammin-pointwise (a b)
  (if (= a b)
      0
    1))

(defun hamming-distance (seq1 seq2)
  (unless (length-equal-p seq1 seq2)
    error "hamming-distance: undefined.")
  (loop for a in (string-to-list seq1)
        for b in (string-to-list seq2)
        sum (hamming-pointwise a b)))


(provide 'point-mutations)
;;; point-mutations.el ends here
