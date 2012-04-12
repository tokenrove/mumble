;;;
;;; YMamoto conversion functions for mumble output.  These functions
;;; produce hisoft-style assembly output, which can be assembled into
;;; a binary playable by the ymamoto playroutine.
;;;
;;; Julian Squires <tek@wiw.org> / 2004
;;;

(in-package :mumble)

(defparameter *ymamoto-frequency* 50)
;; XXX: A lot of these global variables will disappear soon; I'm just lazy.
(defvar *channel-delta* 0)
(defvar *total-frames* 0)
(defvar *total-bytes* 0)
(defvar *loop-point* nil)

;;;; UTILITIES

(defun find-and-remove-loop (list)
  "Finds :loop in the list, and returns two values, the list with the
:loop removed, and the position of the loop.  Does not support
multiple loops."
  (aif (position :loop list)
       (values (remove :loop list) it)
       (values list 0)))

(defun make-env-follow-command (options)
  (make-instance 'music-command :type :envelope-follow :value options))


;;;; INPUT-RELATED FUNCTIONS

(defun make-ymamoto-channels ()
  (list
   (make-channel)
   (make-channel)
   (make-channel)))


(defun ymamoto-special-handler (stream channels)
  (let ((special-char (read-char stream)))
    (cond ((char= special-char #\e)
	   ;; env follow
	   (let ((next-char (peek-char nil stream)))
	     (cond ((char= next-char #\o)
		    (read-char stream)
		    (dolist (c channels)
		      (vector-push-extend (make-env-follow-command :octave)
					  (data-stream-of c))))
		   ((char= next-char #\u)
		    (read-char stream)
		    (dolist (c channels)
		      (vector-push-extend (make-env-follow-command :unison)
					  (data-stream-of c))))
		   ((char= next-char #\0)
		    (read-char stream)
		    (dolist (c channels)
		      (vector-push-extend (make-env-follow-command :disable)
					  (data-stream-of c))))
		   (t (format t "~&Ignored bad env-follow: %e~A"
			      next-char)))))
	  ;; Something else?
	  (t (format t "~&Ignored special invocator: %~A" special-char)))))


;;;; OUTPUT FUNCTIONS

(defun ymamoto-output-note-helper (note-word frames stream
				   &optional (comma nil))
  (incf *channel-delta* frames)
  (multiple-value-bind (frames leftovers) (floor *channel-delta*)
    (setf *channel-delta* leftovers)
    (setf (ldb (byte 7 8) note-word) (1- frames))

    (when (plusp frames)
      (incf *total-frames* frames)
      (incf *total-bytes* 2)
      (format stream (if comma ", $~X" "~&~8TDC.W $~X") note-word))))


(defun ymamoto-output-note (note channel stream)
  (let ((note-word 0)
	(frames (duration-to-frames (duration-of note)
				    (tempo-of channel)
				    *ymamoto-frequency*))
	(staccato-frames 0))

    (cond ((eql (tone-of note) :rest)
	   (setf (ldb (byte 7 0) note-word) 127))
	  ((eql (tone-of note) :wait)
	   (setf (ldb (byte 7 0) note-word) 126))
	  (t
	   (when (/= (staccato-of channel) 1)	   
	     (setf staccato-frames (- frames (* frames
						(staccato-of channel))))
	     (when (< (- frames staccato-frames) 1)
	       (decf staccato-frames))
	     (setf frames (- frames staccato-frames)))

	   (setf (ldb (byte 7 0) note-word) (tone-of note))))

    (ymamoto-output-note-helper note-word frames stream)
    (when (plusp staccato-frames)
      (ymamoto-output-note-helper 127 staccato-frames stream t))))


(defun ymamoto-output-note-stream (notes channel stream)
  "Traverse a note-stream, keeping track of tempo and staccato
  settings, and output assembly directives for this note stream."
  (setf *channel-delta* 0
	*total-frames* 0
	*total-bytes* 0)
  (do* ((note-> 0 (1+ note->))
	note
	(channel-pos 0 (1+ channel-pos)))
       ((>= note-> (length notes)))
    (setf note (aref notes note->))
    (case (music-command-type note)
      (:note (ymamoto-output-note note channel stream))
      (:arpeggio
       (format stream "~&~8TDC.W $~X"
	       (logior (ash #b11000001 8) (value-of note)))
       (incf *total-bytes* 2))
      (:tempo
       (setf (tempo-of channel) (value-of note)))
      (:staccato
       (setf (staccato-of channel) (value-of note)))
      (:volume
       (setf (volume-of channel) (value-of note))
       (format stream "~&~8TDC.W $~X"
	       (logior (ash #b11000011 8) (value-of note)))
       (incf *total-bytes* 2))
      (:volume-envelope
       (format stream "~&~8TDC.W $~X"
	       (logior (ash #b11000100 8) (value-of note)))
       (incf *total-bytes* 2))
      (:vibrato
       (format stream "~&~8TDC.W $~X"
	       (logior (ash #b11001011 8) (value-of note)))
       (incf *total-bytes* 2))
      (:envelope-follow
       (format stream "~&~8TDC.W $~X"
	       (logior (ash #b11001000 8)
		       (ecase (value-of note)
			 (:disable 0)
			 (:unison 1)
			 (:octave #b11)))))
      (t (format t "~&WARNING: YMamoto ignoring ~A."
		 (music-command-type note))))
    (when (and (loop-point-of channel)
	       (= (loop-point-of channel)
		  channel-pos))
      (setf *loop-point* *total-bytes*)))
  (format t "~&frames: ~A, bytes: ~A" *total-frames* *total-bytes*))


(defun output-ymamoto-header (stream)
  (format stream ";;; test song, in assembler form

	ORG 0
song_header:
        DC.W arpeggio_table>>2
        DC.W venv_table>>2
        DC.W vibrato_table>>2"))


(defun ymamoto-output-length-loop-list-table (stream name table)
  ;; note that the zeroth element of the table is skipped.
  (format stream "~&~8TALIGN 4~&~A:~%~8TDC.B ~D" name
	  (max 0 (1- (length table))))
  (do ((i 1 (1+ i)))
      ((>= i (length table)))
    (multiple-value-bind (list loop) (find-and-remove-loop (aref table i))
      (format stream "~&~8TDC.B ~A, ~A~{, ~D~}" (length list) loop list))))

(defun ymamoto-output-vibrato-table (stream table)
  ;; note that the zeroth element of the table is skipped.
  (format stream "~&~8TALIGN 4~&vibrato_table:~%~8TDC.B ~D"
	  (max 0 (1- (length table))))
  (do ((i 1 (1+ i)))
      ((>= i (length table)))
    (flet ((get-field (list field)
	     (nth (1+ (or (position field list)
			  (error "Vibrato ~A lacks ~A!" i field))) list)))
      (let* ((list (aref table i))
	     (delay (get-field list 'DELAY))
	     (depth (get-field list 'DEPTH))
	     (speed (get-field list 'SPEED)))
	(format stream "~&~8TDC.B 3, ~D, ~D, ~D, ~D" delay depth
		(- 5 speed) (ash 1 (- 5 speed)))))))



;;;; HIGH-LEVEL

(defun ymamoto-output-asm (tune out-file)
  (with-open-file (stream out-file
		   :direction :output
		   :if-exists :supersede)
    ;; simple header
    (output-ymamoto-header stream)
    (let ((n (length (tracks-of tune))))
      (format stream "~&~8TDC.B 0,~D		; pad, number of tracks"
              n)
      (dotimes (i n) (format stream "~&~8TDC.W track_~D>>2" (1+ i))))
    (ymamoto-output-length-loop-list-table
     stream "arpeggio_table" (tune-get-table tune :arpeggio))
    (ymamoto-output-length-loop-list-table
     stream "venv_table" (tune-get-table tune :volume-envelope))
    (ymamoto-output-vibrato-table stream (tune-get-table tune :vibrato))
    ;; for n tracks
    (let ((track-num 0))
      (dolist (track-channels (tracks-of tune))
        ;; I bet the following could all be reduced to one big format
        ;; statement.  Yuck.
        (incf track-num)                ; starts at 1
        (format stream "~&~8TALIGN 4~&track_~D:" track-num)
        (do ((c track-channels (cdr c))
             (ctr (char-code #\a) (1+ ctr)))
            ((null c))
          (format stream "~&~8TDC.W channel_~A~A>>2"
                  track-num (code-char ctr)))

        ;; output channels themselves.
        (do ((c track-channels (cdr c))
             (ctr (char-code #\a) (1+ ctr)))
            ((null c))
          (format t "~&note ~A" (loop-point-of (car c)))
          (format stream "~&~8TALIGN 4~&channel_~A~A:"
                  track-num (code-char ctr))
          (ymamoto-output-note-stream (data-stream-of (car c))
                                      (car c)
                                      stream)
          (if (loop-point-of (car c))
              (format stream "~&~8TDC.W $8001, $~X" *loop-point*)
              (format stream "~&~8TDC.W $8000")))))))

(register-replay "YMamoto"
		 #'ymamoto-special-handler
		 #'make-ymamoto-channels
		 #'ymamoto-output-asm)
