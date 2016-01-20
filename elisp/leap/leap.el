;;; leap.el --- Leap exercise (exercism)

;;; Commentary:

;;; Code:
(defun leap-year-p (year)
  (when (divisible-by-p year 4)
    (unless (and (divisible-by-p year 100)
                 (not (divisible-by-p year 400))) t)))

(defun divisible-by-p (x y)
  (= (mod x y) 0))


(provide 'leap)
;;; leap.el ends here
