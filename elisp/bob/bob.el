;;; bob.el --- Bob exercise (exercism)

;;; Commentary:

;;; Code:
(defun response-for (input)
  (cond
   ((and (string-match "[[:alpha:]]" input)
         (string= (upcase input) input))
    "Whoa, chill out!")
   ((string-suffix-p "?" input)
    "Sure.")
   ((string-match "^[[:space:]]*$" input)
    "Fine. Be that way!")
   ("Whatever.")))

(provide 'bob)
;;; bob.el ends here
