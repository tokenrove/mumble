;;;; CLASSES AND DATA STRUCTURES.
;;; (and elementary helper functions associated with specific classes.)
;;;
;;; Julian Squires <tek@wiw.org> / 2004

(in-package :mumble)

(defun make-duration (denom)
  (when denom (/ 1 denom)))


(defclass music-command ()
  ((type :reader music-command-type)
   (value :reader music-command-value)))

(defun make-tempo-command (tempo)
  (let ((cmd (make-instance 'music-command)))
    (setf (slot-value cmd 'type) :tempo)
    (setf (slot-value cmd 'value) tempo)
    cmd))

(defun make-staccato-command (staccato)
  (let ((cmd (make-instance 'music-command)))
    (setf (slot-value cmd 'type) :staccato)
    (setf (slot-value cmd 'value) staccato)
    cmd))

(defun make-simple-volume-command (n)
  (let ((cmd (make-instance 'music-command)))
    (setf (slot-value cmd 'type) :volume)
    (setf (slot-value cmd 'value) n)
    cmd))

;; This might become a special macro-command later.
(defun make-arpeggio-command (n)
  (let ((cmd (make-instance 'music-command)))
    (setf (slot-value cmd 'type) :arpeggio)
    (setf (slot-value cmd 'value) n)
    cmd))

;; This might become a special macro-command later.
(defun make-volume-envelope-command (n)
  (let ((cmd (make-instance 'music-command)))
    (setf (slot-value cmd 'type) :volume-envelope)
    (setf (slot-value cmd 'value) n)
    cmd))

;; This might become a special macro-command later.
(defun make-vibrato-command (n)
  (let ((cmd (make-instance 'music-command)))
    (setf (slot-value cmd 'type) :vibrato)
    (setf (slot-value cmd 'value) n)
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
   (volume :accessor channel-volume)
   (duration :accessor channel-default-duration)
   (loop-point :accessor channel-loop-point)
   ;; repeats is kind of an ugly kludge.
   (repeats :accessor channel-repeats)
   (data-stream :accessor channel-data-stream)))

(defun make-channel ()
  (let ((channel (make-instance 'channel)))
    (setf (channel-octave channel) *default-octave*
	  (channel-tempo channel) *default-tempo*
	  (channel-staccato channel) *default-staccato*
	  (channel-default-duration channel) *default-duration*
	  (channel-volume channel) 0
	  (channel-loop-point channel) nil
	  (channel-repeats channel) nil)

    (setf (channel-data-stream channel)
	  (make-array '(0) :adjustable t :fill-pointer 0))
    channel))

(defun channel-current-position (channel)
  (fill-pointer (channel-data-stream channel)))

(defun copy-and-append-channel-data (channel begin end)
  (loop for x from begin to end
       do (vector-push-extend (aref (channel-data-stream channel) x)
			      (channel-data-stream channel))))


(defclass tune ()
  ((channels :accessor tune-channels)
   (replay :accessor tune-replay)
   (tables :accessor tune-tables)
   (metadata :accessor tune-metadata)))

(defun make-tune ()
  (let ((tune (make-instance 'tune)))
    (setf (tune-metadata tune) nil)
    (setf (tune-tables tune) nil)
    tune))

(defun tune-get-table (tune table-sym)
  (cdr (assoc table-sym (tune-tables tune))))

(defun (setf tune-get-table) (value tune table-sym)
  (setf (cdr (assoc table-sym (tune-tables tune))) value))

(defun tune-add-table (tune table-sym)
  (push (cons table-sym (make-array '(0) :initial-element nil
				    :adjustable t))
	(tune-tables tune)))

(defun tune-add-to-table (tune table-sym index entry)
  (let ((table (tune-get-table tune table-sym)))
    (when (>= index (length table))
      (setf table (adjust-array table (list (1+ index))
				:initial-element nil)))
    (when (aref table index)
      (format t "~&WARNING: ~A entry ~A already exists; replacing."
	      table-sym index))
    (setf (aref table index) entry)
    (setf (tune-get-table tune table-sym) table)))
