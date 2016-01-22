;;; robot-name.el --- Robot Name (exercism)

;;; Commentary:
;;
;; Build a robot with a name like AA000, that can be reset
;; to a new name. Instructions are in README.md
;;

;;; Code:

(defvar robots ())

(defun random-char ()
  (format "%c" (+ ?A (random (- ?Z ?A)))))

(defun random-digit ()
  (format "%s" (random 9)))

(defun robot-name (robot)
  (plist-get robot :name))

(defun random-robot-name ()
  (concat (loop repeat 2
                concat (random-char))
          (loop repeat 3
                concat (random-digit))))

(defun known-robot-names ()
  (mapcar 'robot-name robots))

(defun robot-known-p (name)
  (member name (known-robot-names)))

(defun make-robot-name ()
  (loop with tmp
        do (setq tmp (random-robot-name))
        until (not (robot-known-p tmp))
        finally return tmp))

(defun build-robot ()
  (let* ((name (make-robot-name))
         (new-robot `(:name ,name)))
    (push new-robot robots) new-robot))

(defun reset-robot (robot)
  (let (new-name (make-robot-name))
    (plist-put robot :name new-name)))

(provide 'robot-name)
;;; robot-name.el ends here
