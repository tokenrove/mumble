(in-package :mumble)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *replay-map* nil))

(defun register-replay (name special-handler channel-creator output-fn)
  (let ((replay (list name special-handler channel-creator output-fn)))
    (aif (position name *replay-map* :test #'equal :key #'car)
	 (setf (nth it *replay-map*) replay)
	 (push replay *replay-map*))))

(defun set-tune-replay (name tune)
  (dolist (replay *replay-map*)
    (when (equal name (car replay))
      (setf (tune-replay tune) name)))
  (equal (tune-replay tune) name))

(defun replay-output-fn (replay tune file)
  (do ((list *replay-map* (cdr list)))
      ((equal replay (caar list)) (funcall (fourth (car list)) tune file))))

(defun replay-create-channels (replay)
  (do ((list *replay-map* (cdr list)))
      ((equal replay (caar list)) (funcall (third (car list))))))

(defun replay-special-handler (replay stream channels)
  (do ((list *replay-map* (cdr list)))
      ((equal replay (caar list)) (funcall (second (car list))
					   stream channels))))


;;;; HIGH-LEVEL

(defun compile-mumble (in-file out-file)
  (with-open-file (stream in-file)
    (let ((tune (parse-mumble-file stream)))
      (replay-output-fn (tune-replay tune) tune out-file))))