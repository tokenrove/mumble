;;;
;;; YMamoto conversion functions for mumble output.  These functions
;;; produce hisoft-style assembly output, which can be assembled into
;;; a binary playable by the ymamoto playroutine.
;;;
;;; Julian Squires <tek@wiw.org> / 2003
;;;

(in-package :mumble)

(defparameter *ymamoto-frequency* 50)

(defun make-ymamoto-channels ()
  (list
   (make-channel)
   (make-channel)
   (make-channel)))

(defun output-ymamoto-notes (notes stream)
  ;; Traverse a note-stream, keeping track of tempo and staccato
  ;; settings, and output assembly directives for this note stream.
  (let ((channel (make-channel)))
    (dolist (note notes)
      (cond ((eql (music-command-type note) :note)
	     (let ((note-word 0))
	       (setf (ldb (byte 7 0) note-word)
		     (cond ((eql (note-tone note) :rest) 127)
			   ((eql (note-tone note) :wait) 126)
			   (t (note-tone note))))
	       (setf (ldb (byte 7 8) note-word)
		     (round (duration-to-frames (note-duration note)
						(channel-tempo channel)
						*ymamoto-frequency*)))

	       (format stream "~&        DC.W $~X" note-word)))

	    ((eql (music-command-type note) :tempo)
	     (setf (channel-tempo channel) (music-command-value note)))))))

(defun mml-to-ymamoto-file (mml-file out-file)
  (let ((channels (make-ymamoto-channels)))
    (with-open-file (stream mml-file)
      (parse-music-section stream channels))

    (with-open-file (stream out-file
			    :direction :output
			    :if-exists :supersede)
      (format stream ";;; test song, in assembler form

	ORG 0
song_header:
	DC.B 1			; number of tracks
	DC.L track_1		; pointer to track

track_1:
	;; channel pointers
	DC.L channel_a, channel_b, channel_c
	DC.B 0			; initial tempo
")
      (do ((c channels (cdr c))
	   (ctr (char-code #\a) (1+ ctr)))
	  ((null c))
	(format stream "~&channel_~A:" (code-char ctr))
	(output-ymamoto-notes (channel-data-stream (car c)) stream)
	(format stream "~&        DC.W $8000")))))


