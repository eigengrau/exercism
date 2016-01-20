;;; binary.el --- Binary exercise (exercism)

;;; Commentary:

;;; Code:

(defun str-to-list (str)
  (append str nil))

(defun to-decimal (str)
  (loop for exp from 0 to (length str)
        for chr in (reverse (str-to-list str))
        sum (* (string-to-number (concat `(,chr) nil))
               (expt 2 exp))))

(provide 'binary)
;;; binary.el ends here
