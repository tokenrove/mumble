;;;
;;; Several of these functions are very flaky WRT EOF, and that should
;;; eventually be fixed.  This is all just a quick hack.  Most of this
;;; could be converted to a very data-driven style of programming.
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
;;; (an abashed) Julian Squires <tek@wiw.org> / 2004
;;;

(in-package :mumble)

;;;; CONSTANTS AND PARAMETERS.

(defparameter *channel-select-characters* "ABCDEFGHIJ")
(defparameter *duration-digits* "0123456789")
(defparameter *note-characters* "c_d_ef_g_a_b")
(defparameter *whitespace-characters* #(#\Space #\Newline #\Tab))
(defparameter *ws-and-barline-characters* #(#\Space #\Newline #\Tab #\|))

(defconstant +octave-size+ 12)

(defparameter *staccato-base-division* 1/8)
(defparameter *default-duration* (make-duration 4))
(defparameter *default-octave* 4)
(defparameter *default-staccato* 1)
(defparameter *default-tempo* 120)
(defvar *radix* 10)

;;;; LOW-LEVEL PARSE/LEX ROUTINES.

(defun expect-int (stream)
  (do ((i nil (let ((d (digit-char-p (read-char stream))))
	       (if i (+ d (* i *radix*)) d))))
      ((not (digit-char-p (peek-char nil stream nil #\a))) i)))

(defun expect-duration (stream)
  (let ((duration (make-duration (expect-int stream)))
	;; if the next character is a dot, read dots until the next
	;; character is not a dot.
	(dots (loop while (char= #\. (peek-char nil stream))
		    sum 1
		    do (read-char stream))))

    (if duration
        (incf duration (loop for i from 1 upto dots
                             sum (/ duration (ash 1 i))))
        (when (plusp dots)
          (error "Bad duration (relative dots are not allowed).")))

    ;; tie.
    (when (and duration (char= #\^ (peek-char nil stream)))
      (read-char stream)
      (incf duration (expect-duration stream)))
    duration))

(defun read-accidentals (stream)
  (loop until (char/= (peek-char nil stream) #\+ #\-)
	sum (if (char= (read-char stream) #\+) 1 -1)))

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
    (values (if (char= rest-char #\r) :rest :wait) duration)))

(defun expect-channels (stream)
  (do ((next-char #1=(peek-char nil stream) #1#)
       (channels))
      ((not (find next-char *channel-select-characters*)) channels)
    (push (position (read-char stream) *channel-select-characters*) channels)))

(defun eat-whitespace (stream &optional (characters *whitespace-characters*))
  (do () ((not (find (peek-char nil stream) characters)))
    (read-char stream)))

(defun expect-= (stream)
  (eat-whitespace stream)
  (assert (char= (read-char stream) #\=))
  (eat-whitespace stream))

(defun read-numbers-and-loop-macro (stream)
  (assert (char= (read-char stream) #\{))
  (eat-whitespace stream)
  (do ((next-char #1=(peek-char nil stream) #1#)
       list)
      ((char= next-char #\}) (progn (read-char stream)
				    (reverse list)))
    (cond ((char= next-char #\|)
	   (read-char stream)
	   (push :loop list))
	  ((find next-char "0123456789-")
	   (push (read stream) list))
	  (t
	   (read-char stream)
	   (format t "~&Warning: ignored ~A in macro definition."
		   next-char)))
    (eat-whitespace stream)))

(defun read-symbols-macro (stream)
  (assert (char= (read-char stream) #\{))
  (eat-whitespace stream)
  (do ((symbol (read stream) (read stream))
       list)
      ((eql symbol '}) (reverse list))
    (push symbol list)))

(defparameter *macro-table-mapping*
  '((#\a :arpeggio read-numbers-and-loop-macro)
    (#\v :volume-envelope read-numbers-and-loop-macro)
    (#\i :instrument read-symbols-macro)
    (#\~ :vibrato read-symbols-macro)))

(defun read-macro-definition (stream)
  (assert (char= (read-char stream) #\@))
  (let* ((dispatch (read-char stream))
	 (index (expect-int stream))
	 (mapping (find dispatch *macro-table-mapping* :test #'equal
			:key #'first)))
    (expect-= stream)
    (values (second mapping) index (funcall (third mapping) stream))))


(defun handle-simple-volume (stream channels)
  (assert (char= (read-char stream) #\v))
  (let ((next-char (peek-char nil stream)))
    (cond ((find next-char *duration-digits*)
	   (let ((volume (expect-int stream)))
	     (dolist (c channels)
	       (vector-push-extend
		(make-volume-command volume)
		(data-stream-of c))
	       (setf (volume-of c) volume))))
	  ((char= next-char #\+)
	   (read-char stream)
	   (dolist (c channels)
	     (vector-push-extend
	      (make-volume-command (1+ (volume-of c)))
	      (data-stream-of c))))
	  ((char= next-char #\-)
	   (read-char stream)
	   (dolist (c channels)
	     (vector-push-extend
	      (make-volume-command (1- (volume-of c)))
	      (data-stream-of c))))
	  (t (error "~&Bad volume character: v~A" next-char)))))



;;;; HIGH-LEVEL PARSE ROUTINES.

;;; We should really just create a readtable for the use of all the
;;; following routines.  Basically, what's in parse-header-section,
;;; but with the other CL standard macro characters disabled (parens,
;;; single/back quote, comma).
(defun parse-mumble-file (stream)
  (let ((*read-eval* nil)
	(*package* (find-package :mumble))
	(tune (make-tune)))
    ;; Any preamble that occurs before the first section is ignored.
    (parse-comment-section stream)
    (handler-case
	(do ((section (read stream) (read stream)))
	    (nil)
	  ;; Note that the section handler is always responsible for
	  ;; eating the # sign so we don't see it.
	  (ecase section
	    (COMMENT (parse-comment-section stream))
	    (MACROS (parse-macro-section stream tune))
	    (HEADER (parse-header-section stream tune))
	    (MUSIC (parse-music-section stream tune))))
      (end-of-file () tune))))


(defun parse-comment-section (stream)
  (do () ((char= (read-char stream) #\#))))

(defun parse-header-section (stream tune)
  (let ((*readtable* (copy-readtable))
	done-p)
    (set-macro-character #\#
			 (lambda (stream char)
			   (declare (ignore stream char))
			   (setf done-p t)))
    (do ((header (read stream) (read stream)))
	(done-p)
      (let ((argument (read stream)))
	(case header
	  (REPLAY
	   ;; XXX genericize replay stuff
	   (assert (set-tune-replay argument tune))
	   (setf (channels-of tune)
		 (funcall (replay-channel-creator (replay-of tune)))))
	  ((TITLE COMPOSER COPYRIGHT)
	   (push (list header argument) (metadata-of tune))))))))


(defun parse-macro-section (stream tune)
  (eat-whitespace stream)
  (do ((next-char #1=(peek-char nil stream) #1#))
      (nil)
    (cond ((char= next-char #\@)
	   (multiple-value-bind (table index entry)
	       (read-macro-definition stream)
	     (assert (plusp index) ()
		     "Bad index ~A (tables index from 1 -- 0 is the ~
                     \"effect off\" index)." index)
	     (unless (tune-get-table tune table)
	       (tune-add-table tune table))
	     (tune-add-to-table tune table index entry)))

	  ;; Section change.
	  ((char= next-char #\#)
	   (read-char stream)
	   (return))

	  ;; Comment.
	  ((char= next-char #\;)
	   (read-line stream))

	  ;; Something else?
	  (t (format t "~&Ignored character in macro section: ~A (~:*~S)"
		     (read-char stream))))
    (eat-whitespace stream)))

;; possible ``dispatch table'' format for routine below?
#+nil '((#\o
   ((octave (progn (read-char stream) (expect-int stream))))
   (setf (octave-of channel) octave))
  (#\<
   nil
   (decf (octave-of c)))
  (*note-characters*
   ((note-char accidentals duration) (expect-note stream))
   (push (make-note (calculate-tone note-char
				    accidentals
				    (octave-of channel))
		    (clarify-duration duration channel))
    (data-stream-of channel))))

  

(defun parse-music-section (stream tune
			    &optional loop-channels in-loop-p)
  "Reads a music section from stream; returns at EOF or if a section
change is detected.  Writes data and property changes to channels.
Highly intolerant of malformed inputs."
  (eat-whitespace stream)
  (do ((current-channels (and in-loop-p loop-channels))
       (next-char #1=(peek-char nil stream) #1#))
      (nil)
          ;; Channel selection characters.
    (cond ((find next-char *channel-select-characters*)
	   (setf current-channels nil)
	   (dolist (c (expect-channels stream))
	     (assert (< c (length (channels-of tune)))
		     () "Invalid channel for this replay.")
	     (push (nth c (channels-of tune)) current-channels)))

	  ;; Repeats (unrolled loops).
	  ((char= next-char #\[)
	   (assert current-channels () "Command outside channels.")
	   (read-char stream)
	   (dolist (c current-channels)
	     (push (channel-current-position c)
		   (repeats-of c)))
	   (parse-music-section stream tune current-channels t))

	  ((char= next-char #\])
	   (assert (and in-loop-p
			current-channels))
	   (read-char stream)
	   (let ((count (expect-int stream)))
	     (dolist (c current-channels)
	       (let ((begin (pop (repeats-of c)))
		     (end (1- (channel-current-position c))))
		 (dotimes (i (1- count))
		   (copy-and-append-channel-data c begin end)))))
	   (return))

	  ;; Octave changes.
	  ((char= next-char #\o)
	   (assert current-channels () "Command outside channels.")
	   (read-char stream)
	   (let ((octave (expect-int stream)))
	     (dolist (c current-channels)
	       (setf (octave-of c) octave))))

	  ((char= next-char #\<)
	   (assert current-channels () "Command outside channels.")
	   (read-char stream)
	   (dolist (c current-channels)
	     (decf (octave-of c))))

	  ((char= next-char #\>)
	   (assert current-channels () "Command outside channels.")
	   (read-char stream)
	   (dolist (c current-channels)
	     (incf (octave-of c))))

	  ;; (Non-venv) volume changes.
	  ((char= next-char #\v)
	   (assert current-channels () "Command outside channels.")
	   (handle-simple-volume stream current-channels))

	  ;; Notes and rests.
	  ((find next-char *note-characters*)
	   (assert current-channels () "Command outside channels.")
	   (multiple-value-bind (note-char accidentals duration)
	       (expect-note stream)
	     (dolist (c current-channels)
	       (vector-push-extend (make-note
				    (calculate-tone note-char
						    accidentals
						    (octave-of c))
				    (clarify-duration duration c))
				   (data-stream-of c)))))

	  ((or (char= next-char #\r) (char= next-char #\w))
	   (assert current-channels () "Command outside channels.")
	   (multiple-value-bind (note-type duration)
	       (expect-rest stream)
	     (dolist (c current-channels)
	       (vector-push-extend (make-note note-type
					      (clarify-duration duration c))
				   (data-stream-of c)))))

	  ;; Tempo change.
	  ((char= next-char #\t)
	   (assert current-channels () "Command outside channels.")
	   (read-char stream)
	   (let ((tempo (expect-int stream)))
	     (dolist (c current-channels)
	       (vector-push-extend (make-tempo-command tempo)
				   (data-stream-of c))
	       (setf (tempo-of c) tempo))))
	  
	  ;; Section change.
	  ((char= next-char #\#)
	   (read-char stream)
	   (when in-loop-p
	     (warn "Changing sections during a [] repeat.  ~
                    This probably won't work."))
	   (return))

	  ;; Staccato.
	  ((char= next-char #\q)
	   (assert current-channels () "Command outside channels.")
	   (read-char stream)
	   (let ((staccato (* *staccato-base-division* (expect-int stream))))
	     (dolist (c current-channels)
	       (vector-push-extend (make-staccato-command staccato)
				   (data-stream-of c))
	       (setf (staccato-of c) staccato))))

	  ;; Macro invocation.
	  ((char= next-char #\@)
	   (assert current-channels () "Command outside channels.")
	   (parse-macro-invocation stream current-channels))

	  ;; Structural dispatch character.
	  ((char= next-char #\!)
	   (assert current-channels () "Command outside channels.")
	   (parse-bang-invocation stream current-channels))

	  ;; Replay-special invocation.
	  ((char= next-char #\%)
	   (assert current-channels () "Command outside channels.")
	   (read-char stream)
	   (funcall (replay-special-handler (replay-of tune))
		    stream current-channels))

	  ;; Comment.
	  ((char= next-char #\;)
	   (read-line stream))

	  ;; Something else?
	  (t (format t "~&Ignored character in music section: ~A (~:*~S)"
		     (read-char stream))))
    (eat-whitespace stream *ws-and-barline-characters*)))


;;; XXX: should use *macro-table-mapping*
(defun parse-macro-invocation (stream channels)
  (read-char stream)
  (let ((next-char (peek-char nil stream)))
          ;; Arpeggio.
    (cond ((char= next-char #\a)
	   (read-char stream)
	   (let ((arp-num (expect-int stream)))
	     (dolist (c channels)
	       (vector-push-extend (make-arpeggio-command arp-num)
				   (data-stream-of c)))))
	  ;; Volume envelope.
	  ((char= next-char #\v)
	   (read-char stream)
	   (let ((venv-num (expect-int stream)))
	     (dolist (c channels)
	       (vector-push-extend (make-volume-envelope-command venv-num)
				   (data-stream-of c)))))

	  ;; Vibrato.
	  ((char= next-char #\~)
	   (read-char stream)
	   (let ((vibrato-num (expect-int stream)))
	     (dolist (c channels)
	       (vector-push-extend (make-vibrato-command vibrato-num)
				   (data-stream-of c)))))

	  ;; Something else?
	  (t (format t "~&Ignored macro invocator: @~A (~:*~S)"
		     (read-char stream))))))


(defun parse-bang-invocation (stream channels)
  (assert (char= (read-char stream) #\!))
  (let* ((symbol (read stream)))
    (ecase symbol
      (LOOP
	 (dolist (c channels)
	   (setf (loop-point-of c) (channel-current-position c))))
      (END
       (format t "~&I'm afraid !end is currently unsupported.")
       ;;; XXX how to handle this nicely?
       #+nil(dolist (c channels)
	      (vector-push-extend (make-track-end-command)
				  (data-stream-of c)))))))
