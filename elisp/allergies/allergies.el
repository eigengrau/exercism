;;; allergies.el --- Allergies Exercise (exercism)

;;; Commentary:

;;; Code:
(require 'cl)

(defvar foods '(eggs peanuts shellfish strawberries tomatoes
  chocolate pollen cats))

(defun powers2 (n)
  (loop for i from 0 to (- n 1)
        collect (expt 2 i)))

(defvar allergens
  (loop for food in foods
        for coded in (powers2 (length foods))
        collect (cons food coded)))

(defun allergen-list (score)
  (reverse
   (remove nil (loop for (food . code) in (reverse allergens)
                     collect (when (>= score code)
                               (while (>= score code)
                                 (setq score (- score code)))
                               (symbol-name food))))))

(provide 'allergies)
;;; allergies.el ends here
