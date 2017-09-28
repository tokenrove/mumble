;;;
;;; YMamoto conversion functions for mumble output.  These functions
;;; produce hisoft-style assembly output, which can be assembled into
;;; a binary playable by the ymamoto playroutine.
;;;
;;; Julian Squires <tek@wiw.org> / 2004
;;;

(defpackage :mumble-ymamoto
  (:use :cl :mumble :alexandria :anaphora)
  (:import-from :mumble
   :tempo-of :volume-of :staccato-of :loop-point-of :tracks-of :music-command-type
   :tune-get-table :data-stream-of :make-channel :duration-to-frames :duration-of
   :tone-of :value-of))
(in-package :mumble-ymamoto)

(defparameter *frequency* 50)
;; XXX: A lot of these global variables will disappear soon; I'm just lazy.
(defvar *channel-delta* 0)
(defvar *total-frames* 0)
(defvar *total-bytes* 0)
(defvar *loop-point* nil)

(defparameter *table-alignment* 4)

;;;; UTILITIES

(defun find-and-remove-loop (list)
  "Finds :loop in the list, and returns two values, the list with the
:loop removed, and the position of the loop.  Does not support
multiple loops."
  (aif (position :loop list)
       (values (remove :loop list) it)
       (values list 0)))

(defun make-env-follow-command (options)
  (make-instance 'mumble::music-command :type :envelope-follow :value options))

(defun emit-byte (value vector)
  (vector-push-extend (ldb (byte 8 0) value) vector))

(defun ensure-alignment (vector alignment)
  (let ((n (mod (fill-pointer vector) alignment)))
    (when (/= 0 n)
      (dotimes (i (- alignment n)) (emit-byte 0 vector)))))

(defun emit-16u-be (word vector)
  (emit-byte (ldb (byte 8 8) word) vector)
  (emit-byte (ldb (byte 8 0) word) vector))
(defun write-16u-be (word stream)
  (write-byte (ldb (byte 8 8) word) stream)
  (write-byte (ldb (byte 8 0) word) stream))

;;;; INPUT-RELATED FUNCTIONS

(defun make-channels ()
  (list
   (make-channel)
   (make-channel)
   (make-channel)))


(defun special-handler (stream channels)
  (let ((special-char (read-char stream)))
    (cond ((char= special-char #\e)
           ;; env follow
           (let ((next-char (peek-char nil stream)))
             (let ((kind (ecase next-char (#\o :octave) (#\u :unison) (#\0 :disable))))
               (read-char stream)
               (dolist (c channels)
                 (vector-push-extend (make-env-follow-command kind)
                                     (data-stream-of c))))))
          ;; Something else?
          (t (format t "~&Ignored special invocator: %~A" special-char)))))


;;;; OUTPUT FUNCTIONS

(defun output-note-helper (note-word frames stream)
  (incf *channel-delta* frames)
  (multiple-value-bind (frames leftovers) (floor *channel-delta*)
    (setf *channel-delta* leftovers)
    (setf (ldb (byte 7 8) note-word) (1- frames))

    (when (plusp frames)
      (incf *total-frames* frames)
      (incf *total-bytes* 2)
      (emit-16u-be note-word stream))))


(defun output-note (note channel stream)
  (let ((note-word 0)
        (frames (duration-to-frames (duration-of note)
                                    (tempo-of channel)
                                    *frequency*))
        (staccato-frames 0))

    (acond ((find (tone-of note) #(:rest :wait))
            (setf (ldb (byte 7 0) note-word) (if (eql it :rest) 127 126)))
      (t
       (when (/= (staccato-of channel) 1)
         (setf staccato-frames (- frames (* frames (staccato-of channel))))
         (when (< (- frames staccato-frames) 1)
           (decf staccato-frames))
         (setf frames (- frames staccato-frames)))
       (setf (ldb (byte 7 0) note-word) (tone-of note))))

    (output-note-helper note-word frames stream)
    (when (plusp staccato-frames)
      (output-note-helper 127 staccato-frames stream))))


(defun output-note-stream (notes channel stream)
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
      (:note (output-note note channel stream))
      (:arpeggio
       (emit-16u-be (logior (ash #b11000001 8) (value-of note)) stream)
       (incf *total-bytes* 2))
      (:tempo
       (setf (tempo-of channel) (value-of note)))
      (:staccato
       (setf (staccato-of channel) (value-of note)))
      (:volume
       (setf (volume-of channel) (value-of note))
       (emit-16u-be (logior (ash #b11000011 8) (value-of note)) stream)
       (incf *total-bytes* 2))
      (:volume-envelope
       (emit-16u-be (logior (ash #b11000100 8) (value-of note)) stream)
       (incf *total-bytes* 2))
      (:vibrato
       (emit-16u-be (logior (ash #b11001011 8) (value-of note)) stream)
       (incf *total-bytes* 2))
      (:envelope-follow
       (emit-16u-be (logior (ash #b11001000 8)
                             (ecase (value-of note)
                               (:disable #b00)
                               (:unison  #b01)
                               (:octave  #b11)))
                     stream))
      (t (format t "~&WARNING: YMamoto ignoring ~A."
                 (music-command-type note))))
    (when (and (loop-point-of channel)
               (= (loop-point-of channel)
                  channel-pos))
      (setf *loop-point* *total-bytes*)))
  (format t "~&frames: ~A, bytes: ~A" *total-frames* *total-bytes*))


;;;; HIGH-LEVEL

(defun output-vibrato-table (stream table)
  ;; note that the zeroth element of the table is skipped.
  (let ((n (length table)))
    (emit-byte (max 0 (1- n)) stream)
    (loop for i from 1 below n
          do (let* ((list (aref table i))
                    (speed (getf list 'mumble::SPEED)))
               (mapc (lambda (b) (emit-byte b stream))
                     (list 3 (getf list 'mumble::DELAY) (getf list 'mumble::DEPTH) (- 5 speed) (ash 1 (- 5 speed)))))))
  (ensure-alignment stream *table-alignment*))

(defun output-length-loop-list-table (stream table)
  ;; note that the zeroth element of the table is skipped.
  (let ((n (length table)))
    (emit-byte (max 0 (1- n)) stream)
    (loop for i from 1 below n
          do (multiple-value-bind (list loop) (find-and-remove-loop (aref table i))
               (emit-byte (length list) stream)
               (emit-byte loop stream)
               (dolist (e list) (emit-byte e stream)))))
  (ensure-alignment stream *table-alignment*))

(defun output-bin (tune out-file)
  (macrolet ((aggregate (&body body)
               `(let ((s (make-array 0 :element-type 'unsigned-byte :adjustable t :fill-pointer 0)))
                  (prog1 s ,@body))))
    ;; create bytes of all tables (incl. alignment bytes)
    (let ((vibratos (aggregate (output-vibrato-table s (tune-get-table tune :vibrato))))
          (arps (aggregate (output-length-loop-list-table s (tune-get-table tune :arpeggio))))
          (venvs (aggregate (output-length-loop-list-table s (tune-get-table tune :volume-envelope))))
          (n-tracks (length (tracks-of tune)))
          (tracks (mapcar (lambda (x) (mapcar (lambda (c)
                                           (aggregate
                                            (output-note-stream (data-stream-of c) c s)
                                            (emit-16u-be (if (loop-point-of c) #x8001 #x8000) s)
                                            (when (loop-point-of c) (emit-16u-be *loop-point* s))))
                                         x))
                          (tracks-of tune))))
      ;; create bytes of all channels of all tracks
      (with-open-file (stream out-file
                              :element-type 'unsigned-byte
                              :direction :output
                              :if-exists :supersede)
        (let ((-> (* 2 (+ 4 n-tracks))))
          (flet ((pointer-to (object)
                   (write-16u-be (ash -> -2) stream)
                   (incf -> (if (numberp object) object (length object)))))
            ;; write header
            (mapc #'pointer-to (list arps venvs vibratos))
            (write-byte 0 stream)           ; padding
            (write-byte n-tracks stream)
            (let ((track->s (mapcar (lambda (track) (pointer-to (+ (* 2 (length track))
                                                            (reduce #'+ track :key #'length)))) tracks)))
              (mapc (lambda (x) (write-sequence x stream)) (list arps venvs vibratos))
              (mapc (lambda (track track->)
                      (setf -> track->)
                      (mapc #'pointer-to track)
                      (mapc (lambda (channel) (write-sequence channel stream)) track))
                    tracks track->s))))))))

(mumble:register-replay "YMamoto"
                        #'special-handler
                        #'make-channels
                        #'output-bin)
