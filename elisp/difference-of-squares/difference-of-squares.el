;;; difference-of-squares.el --- Difference of Squares (exercism)
(require 'cl)
;;; Commentary:

;;; Code:
(defun square-of-sums (n)
  (let ((sum (loop for i
                   from 1 to n
                   sum i)))
    (expt sum 2)))

(defun sum-of-squares (n)
  (loop for i
        from 1 to n
        sum (expt i 2)))

(defun difference (n)
  (- (square-of-sums n) (sum-of-squares n)))

(provide 'difference-of-squares)
;;; difference-of-squares.el ends here
