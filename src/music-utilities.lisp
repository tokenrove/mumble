
(in-package :mumble)

;; 60 seconds in a minute, 4 beats per whole note.
(defconstant +seconds-per-minute+ 60)
(defconstant +beats-per-whole-note+ 4)

(defun duration-to-frames (duration tempo &optional (frequency 50))
  "Returns a /fractional/ duration -- the conversion routine is
responsible for dealing with these fractions as it sees fit."
  (* (/ (* frequency +seconds-per-minute+)
	(/ tempo +beats-per-whole-note+))
     duration))

(defun clarify-duration (duration channel)
  (if duration
      (setf (default-duration-of channel) duration)
      (default-duration-of channel)))

(defun calculate-tone (char accidentals octave)
  (+ (* +octave-size+ octave)
     (position char *note-characters*)
     accidentals))

