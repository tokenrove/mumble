;;;
;;; Several of these functions are very flaky WRT EOF, and that should
;;; eventually be fixed.  This is all just a quick hack.
;;;
;;; Other things that should be checked/fixed:
;;;     - durations should get tweaked (say, by parse-music-section) if
;;;       we're inside a triplet or tuplet figure.
;;;     - haven't figured out yet who should deal with specifying an
;;;       initial tempo if we don't find one before the first note.  I
;;;       have a feeling I should just have this code insert a tempo
;;;       set event on any channel where we get a duration-dependant
;;;       event before any tempo is set.
;;;
;;; (an abashed) Julian Squires <tek@wiw.org> / 2003
;;;

(in-package :mumble)

;;;; CONSTANTS AND PARAMETERS.

(defparameter *channel-select-characters* "ABCDEFGHIJ")
(defparameter *duration-digits* "0123456789")
(defparameter *note-characters* "c_d_ef_g_a_b")
(defparameter *whitespace-characters* #(#\Space #\Newline #\|))

(defconstant +octave-size+ 12)

(defparameter *default-duration* (make-duration 4))
(defparameter *default-octave* 4)
(defparameter *default-tempo* 120)


;;;; CLASSES AND DATA STRUCTURES.

(defclass duration ()
  ((denominator :reader duration-denominator)
   ;; other modifiers here
   (dots :reader duration-dots)))

(defun make-duration (denominator &optional (dots 0))
  (when denominator
    (let ((duration (make-instance 'duration)))
      (setf (slot-value duration 'denominator) denominator)
      (setf (slot-value duration 'dots) dots)
      duration)))

(defmethod print-object ((obj duration) stream)
  (print-unreadable-object (obj stream :type t)
    (princ (duration-denominator obj) stream)
    (dotimes (i (duration-dots obj))
      (princ #\. stream))))


(defclass music-command ()
  ((type :reader music-command-type)
   (value :reader music-command-value)))

(defun make-tempo-command (tempo)
  (let ((cmd (make-instance 'music-command)))
    (setf (slot-value cmd 'type) :tempo)
    (setf (slot-value cmd 'value) tempo)
    cmd))


(defclass note (music-command)
  ((tone :reader note-tone)
   (duration :reader note-duration))
  (:documentation "Notes encapsulate an absolute pitch (the TONE slot)
and a relative length (the DURATION slot).  DURATION is relative to
the current channel tempo."))

(defun make-note (tone duration)
  (let ((note (make-instance 'note)))
    (setf (slot-value note 'type) :note)
    (setf (slot-value note 'tone) tone)
    (setf (slot-value note 'duration) duration)
    note))

(defmethod print-object ((obj note) stream)
  (print-unreadable-object (obj stream :type t)
    (princ (note-tone obj) stream)
    (princ #\Space stream)
    (princ (note-duration obj) stream)))


(defclass channel ()
  ((octave :accessor channel-octave)
   (tempo :accessor channel-tempo)
   (staccato :accessor channel-staccato)
   (duration :accessor channel-default-duration)
   (loop-point :accessor channel-loop-point)
   (data-stream :accessor channel-data-stream)))

(defun make-channel ()
  (let ((channel (make-instance 'channel)))
    (setf (channel-octave channel) *default-octave*)
    (setf (channel-tempo channel) *default-tempo*)
    (setf (channel-default-duration channel) *default-duration*)
    (setf (channel-data-stream channel) nil)
    channel))



;;;; LOW-LEVEL PARSE/LEX ROUTINES.

(defun digit-to-int (char)
  (- (char-code char) (char-code #\0)))

(defun clarify-duration (duration channel)
  (if duration
      (setf (channel-default-duration channel) duration)
      (channel-default-duration channel)))

(defun expect-int (stream)
  ;; if the next character is a digit, read digits until the next
  ;; character is not a digit.
  (do ((next-char #1=(peek-char nil stream) #1#)
       (int nil))
      ((not (find next-char *duration-digits*)) int)
    (let ((digit (digit-to-int (read-char stream))))
      (if int
	  (setf int (+ (* int 10) digit))
	  (setf int digit)))))

(defun expect-duration (stream)
  (let ((duration (make-duration (expect-int stream)))
	;; if the next character is a dot, read dots until the next
	;; character is not a dot.
	(dots (do ((next-char #2=(peek-char nil stream) #2#)
		   (number-of-dots 0 (1+ number-of-dots)))
		  ((char/= next-char #\.) number-of-dots)
		(read-char stream))))

    (when (plusp dots)
      (setf (slot-value duration 'dots) dots))
    duration))

(defun calculate-tone (char accidentals octave)
  (let ((tone-value (* +octave-size+ octave)))
    (incf tone-value
	  (do ((i 0 (1+ i)))
	      ((char= char (schar *note-characters* i)) i)
	    (assert (< i (length *note-characters*)))))
    (incf tone-value accidentals)
    tone-value))

(defun read-accidentals (stream)
  (do ((next-char #1=(peek-char nil stream) #1#)
       (accidentals 0))
      ((char/= next-char #\+ #\-) accidentals)
    (if (char= (read-char stream) #\+)
	(incf accidentals)
	(decf accidentals))))

(defun expect-note (stream)
  (let* ((note-char (read-char stream))
	 (accidentals (read-accidentals stream))
	 (duration (expect-duration stream)))

    ;; this function should always be called when we know there's a
    ;; note character next.
    (assert (find note-char *note-characters*))

    (values note-char accidentals duration)))

(defun expect-rest (stream)
  (let ((rest-char (read-char stream))
	(duration (expect-duration stream)))

    (if (char= rest-char #\r)
	(values :rest duration)
	(values :wait duration))))

(defun expect-channels (stream)
  (do ((next-char #1=(peek-char nil stream) #1#)
       (channels))
      ((not (find next-char *channel-select-characters*)) channels)
    ;; XXX dumb hack
    (push (- (char-code (read-char stream))
	     (char-code (char *channel-select-characters* 0)))
	  channels)))

(defun eat-whitespace-and-barlines (stream)
  (do ((next-char #1=(peek-char nil stream) #1#))
      ((not (find next-char *whitespace-characters*)))
    (read-char stream)))


(defmacro mv-push (source destination key)
  `(do ((d ,destination (cdr d))
	(s ,source (cdr s)))
    ((not d))
    (push (car s) (,key (car d)))))


;;;; HIGH-LEVEL PARSE ROUTINES.

(defun parse-music-section (stream channels)
  "Reads a music section from stream; returns at EOF or if a section
change is detected.  Writes data and property changes to channels.
Highly intolerant of malformed inputs."
  (handler-case
      (music-parse-internal stream channels)
    (end-of-file ()))
  (dolist (c channels)
    (setf (channel-data-stream c) (reverse (channel-data-stream c)))))

(defun music-parse-internal (stream channels)
  (do ((current-channels nil)
       (next-char #1=(peek-char nil stream) #1#))
      (nil)
          ;; Channel selection characters.
    (cond ((find next-char *channel-select-characters*)
	   (setf current-channels nil)
	   (dolist (c (expect-channels stream))
	     (push (nth c channels) current-channels)))

	  ;; Octave changes.
	  ((char= next-char #\o)
	   (assert current-channels)
	   (read-char stream)
	   (let ((octave (expect-int stream)))
	     (dolist (c current-channels)
	       (setf (channel-octave c) octave))))

	  ((char= next-char #\<)
	   (assert current-channels)
	   (read-char stream)
	   (dolist (c current-channels)
	     (decf (channel-octave c))))

	  ((char= next-char #\>)
	   (assert current-channels)
	   (read-char stream)
	   (dolist (c current-channels)
	     (incf (channel-octave c))))

	  ;; Notes and rests.
	  ((find next-char *note-characters*)
	   (assert current-channels)
	   (multiple-value-bind (note-char accidentals duration)
	       (expect-note stream)
	     (dolist (c current-channels)
	       (push (make-note (calculate-tone note-char
						accidentals
						(channel-octave c))
				(clarify-duration duration c))
		     (channel-data-stream c)))))

	  ((or (char= next-char #\r) (char= next-char #\w))
	   (assert current-channels)
	   (multiple-value-bind (note-type duration)
	       (expect-rest stream)
	     (dolist (c current-channels)
	       (push (make-note note-type
				(clarify-duration duration c))
		     (channel-data-stream c)))))

	  ;; Tempo change.
	  ((char= next-char #\t)
	   (assert current-channels)
	   (read-char stream)
	   (let ((tempo (expect-int stream)))
	     (dolist (c current-channels)
	       (push (make-tempo-command tempo)
		     (channel-data-stream c))
	       (setf (channel-tempo c) tempo))))
	  ((char= next-char #\#)
	   (return))

	  ;; Something else?
	  (t (format nil "~&Ignored character: ~A"
		     (read-char stream))))
    (eat-whitespace-and-barlines stream)))
