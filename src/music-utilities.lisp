
(in-package :mumble)

;; 60 seconds in a minute, 4 beats per whole note.
(defconstant +seconds-per-minute+ 60)
(defconstant +beats-per-whole-note+ 4)

(defun duration-to-frames (duration tempo &optional (frequency 50))
  "Returns a /fractional/ duration -- the conversion routine is
responsible for dealing with these fractions as it sees fit."
  (let ((count (* (/ (* frequency +seconds-per-minute+)
		     (/ tempo +beats-per-whole-note+))
		  duration)))
    count))

(defun clarify-duration (duration channel)
  (if duration
      (setf (channel-default-duration channel) duration)
      (channel-default-duration channel)))

(defun calculate-tone (char accidentals octave)
  (let ((tone-value (* +octave-size+ octave)))
    (incf tone-value
	  (do ((i 0 (1+ i)))
	      ((char= char (schar *note-characters* i)) i)
	    (assert (< i (length *note-characters*)))))
    (incf tone-value accidentals)
    tone-value))

