
(in-package :mumble)

(defconstant +seconds-per-minute+ 60)

(defun duration-to-frames (duration tempo &optional (frequency 50))
  "Returns a /fractional/ duration -- the conversion routine is
responsible for dealing with these fractions as it sees fit."
  (let ((count (/ (/ frequency (/ tempo +seconds-per-minute+))
		  (duration-denominator duration))))
    ;; dots
    (do ((dots (duration-dots duration) (1- dots))
	 (extra (/ count 2) (/ extra 2)))
	((not (plusp dots)))
      (incf count extra))
    ;; XXX staccato, ties
    count))
