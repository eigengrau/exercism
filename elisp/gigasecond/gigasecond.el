;;; gigasecond.el --- Gigasecond exercise (exercism)

;;; Commentary:
;; Calculate the date one gigasecond (10^9 seconds) from the
;; given date.
;;
;; NB: Pay attention to  Emacs' handling of time zones and dst
;; in the encode-time and decode-time functions.

;;; Code:
(defun from (s min h d m y)
  (let* ((old-tz (getenv "TZ"))
         (birth (encode-time s min h d m y "UTC"))
         (gigasec (time-add birth (seconds-to-time (expt 10 9)))))
    (setenv "TZ" "UTC")
    (let ((result (subseq (decode-time gigasec) 0 6)))
      (setenv "TZ" old-tz)
      result)))


(provide 'gigasecond)
;;; gigasecond.el ends here
