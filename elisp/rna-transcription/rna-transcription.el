;;; rna-transcription.el -- RNA Transcription (exercism)

;;; Commentary:


;;; Code:
(defun list-to-string (l)
  (apply 'concat (mapcar 'string l)))

(defun to-rna (string)
  (list-to-string (mapcar 'transcribe-char string)))

(defvar transcription-table
  '((?G . ?C)
    (?C . ?G)
    (?T . ?A)
    (?A . ?U)))

(defun transcribe-char (char)
  (cdr (assoc char transcription-table)))

(provide 'rna-transcription)
;;; rna-transcription.el ends here
