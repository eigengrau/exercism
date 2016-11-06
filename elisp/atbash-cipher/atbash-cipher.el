;;; atbash-cipher.el --- Atbash-Cipher (exercism)

;;; Commentary:

;;; Code:
(require 'cl)
(require 's)
(require 'dash)

(defvar cipher-table
  (append
   (loop for plain from ?a to ?z
         for cipher downfrom ?z to ?a
         collect `(,plain . ,cipher))
   (loop for plain from ?0 to ?9
         collect `(,plain . ,plain))))

(defun cipher-char (plain-char)
  (cdr (assoc plain-char cipher-table)))

(defun encode (plaintext)
  (let ((plaintext (downcase (remove ?  plaintext))))
    (s-join " " (-map
                 (-partial 'apply 'string)
                 (-partition-all 5 (remove nil(mapcar 'cipher-char plaintext)))))))

(provide 'atbash-cipher)
;;; atbash-cipher.el ends here
