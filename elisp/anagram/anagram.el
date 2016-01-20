;;; anagram.el --- Anagram (exercism)

;;; Commentary:

;;; Code:

(require 'cl)
(require 'dash)

(defun string-to-list (str)
  (append str nil))

(defun anagrams-for (str candidates)
  (-filter (lambda (candidate) (is-anagram-of str candidate))
           candidates))

(defun is-anagram-of (str candidate)
  (unless (string= str candidate)
    (let ((vocab1 (sort (string-to-list (downcase str)) #'>))
          (vocab2 (sort (string-to-list (downcase candidate)) #'>)))
      (equal vocab1 vocab2))))


(provide 'anagram)
;;; anagram.el ends here
