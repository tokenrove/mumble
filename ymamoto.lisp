;;;
;;; YMamoto conversion functions for mumble output.  These functions
;;; produce hisoft-style assembly output, which can be assembled into
;;; a binary playable by the ymamoto playroutine.
;;;
;;; Julian Squires <tek@wiw.org> / 2004
;;;

(in-package :mumble)

(defparameter *ymamoto-frequency* 50)
(defvar *channel-delta* 0)
(defvar *total-frames* 0)

(defun make-ymamoto-channels ()
  (list
   (make-channel)
   (make-channel)
   (make-channel)))


(defun ymamoto-note-output (note channel stream)
  (let ((note-word 0)
	(frames (duration-to-frames (note-duration note)
				    (channel-tempo channel)
				    *ymamoto-frequency*))
	(staccato-frames 0))

    (cond ((eql (note-tone note) :rest)
	   (setf (ldb (byte 7 0) note-word) 127))
	  ((eql (note-tone note) :wait)
	   (setf (ldb (byte 7 0) note-word) 126))
	  (t
	   (when (/= (channel-staccato channel) 1)	   
	     (setf staccato-frames (- frames (* frames
						(channel-staccato channel))))
	     (when (< (- frames staccato-frames) 1)
	       (decf staccato-frames))
	     (setf frames (- frames staccato-frames)))

	   (setf (ldb (byte 7 0) note-word) (note-tone note))))

    (output-note note-word frames stream)
    (when (>= staccato-frames 1)
      (output-note 127 staccato-frames stream t))))

(defun output-note (note-word frames stream &optional (comma nil))
  (incf *channel-delta* frames)
  (multiple-value-bind (frames leftovers) (floor *channel-delta*)
    (setf *channel-delta* leftovers)
    (setf (ldb (byte 7 8) note-word) (1- (floor frames)))

    (unless (< frames 1)
      (incf *total-frames* (floor frames))
      (format stream (if comma ", $~X" "~&        DC.W $~X") note-word))))


(defun output-ymamoto-notes (notes stream)
  ;; Traverse a note-stream, keeping track of tempo and staccato
  ;; settings, and output assembly directives for this note stream.
  (let ((channel (make-channel)))
    (setf *channel-delta* 0)
    (setf *total-frames* 0)
    (dolist (note notes)
      (cond ((eql (music-command-type note) :note)
	     (ymamoto-note-output note channel stream))
	    ((eql (music-command-type note) :arpeggio)
	     (format stream "~&     DC.W $~X"
		     (logior (ash #b11000000 8)
			     (music-command-value note))))
	    ((eql (music-command-type note) :tempo)
	     (setf (channel-tempo channel) (music-command-value note)))
	    ((eql (music-command-type note) :staccato)
	     (setf (channel-staccato channel)
		   (music-command-value note)))))
    (format t "frames: ~A~%" *total-frames*)))


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
        DC.L arpeggio_table     ; pointer to arpeggio table
        DC.L venv_table         ; pointer to volume envelope table
	DC.B 1			; number of tracks
	DC.L track_1		; pointer to track

arpeggio_table:
        DC.B 9			; number of arpeggios
        ; length, loop point, data...
arp_entry_1:
        DC.B 4, 1, 0, 3, 9, -12
arp_entry_2:
        DC.B 4, 1, 0, 5, 7, -12
arp_entry_3:
        DC.B 4, 1, 0, 3, 4, -7
arp_entry_4:
        DC.B 4, 1, 0, 5, 4, -9
arp_entry_5:
        DC.B 4, 1, 0, 5, 3, -8
arp_entry_6:
        DC.B 4, 1, 0, 2, 3, -5
arp_entry_7:
        DC.B 4, 1, 0, 6, 3, -9
arp_entry_8:
        DC.B 4, 1, 0, 4, 3, -7
arp_entry_9:
        DC.B 4, 1, 0, 4, 8, -12

venv_table:
        DC.B 0

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
	(format stream "~&          DC.W $8000")))))


