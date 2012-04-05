;;;; CLASSES AND DATA STRUCTURES.
;;; (and elementary helper functions associated with specific classes.)
;;;
;;; Julian Squires <tek@wiw.org> / 2004

(in-package :mumble)

(defun make-duration (denom)
  (when denom (/ 1 denom)))

(defclass music-command ()
  ((type :reader music-command-type :initarg :type)
   (value :reader value-of :initarg :value)))

(defmacro make-commands (&rest syms)
  `(progn
     ,@(loop for s in syms
	     collect `(defun ,(symbolicate 'make- s '-command) (value)
			(make-instance 'music-command :type ',s :value value)))))

(make-commands tempo staccato volume arpeggio volume-envelope vibrato)

(defclass note (music-command)
  ((tone :reader tone-of :initarg :tone)
   (duration :reader duration-of :initarg :duration))
  (:documentation "Notes encapsulate an absolute pitch (the TONE slot)
and a relative length (the DURATION slot).  DURATION is relative to
the current channel tempo."))

(defun make-note (tone duration)
  (make-instance 'note :type :note :tone tone :duration duration))

(defmethod print-object ((obj note) stream)
  (print-unreadable-object (obj stream :type t)
    (princ (tone-of obj) stream)
    (princ #\Space stream)
    (princ (duration-of obj) stream)))

(defclass channel ()
  ((octave :accessor octave-of :initform *default-octave*)
   (tempo :accessor tempo-of :initform *default-tempo*)
   (staccato :accessor staccato-of :initform *default-staccato*)
   (volume :accessor volume-of :initform 0)
   (duration :accessor default-duration-of :initform *default-duration*)
   (loop-point :accessor loop-point-of :initform nil)
   ;; repeats is kind of an ugly kludge.
   (repeats :accessor repeats-of :initform nil)
   (data-stream :accessor data-stream-of :initarg :data-stream)))

(defun make-channel ()
  (make-instance 'channel :data-stream (make-array '(0) :adjustable t :fill-pointer 0)))

(defun channel-current-position (channel)
  (fill-pointer (data-stream-of channel)))

(defun copy-and-append-channel-data (channel begin end)
  (loop for x from begin to end
       do (vector-push-extend (aref (data-stream-of channel) x)
			      (data-stream-of channel))))


(defclass tune ()
  ((channels :accessor channels-of)
   (replay :accessor replay-of)
   (tables :accessor tables-of :initform nil)
   (metadata :accessor metadata-of :initform nil)))

(defun make-tune () (make-instance 'tune))

(defun tune-get-table (tune table-sym)
  (cdr (assoc table-sym (tables-of tune))))

(defun (setf tune-get-table) (value tune table-sym)
  (setf (cdr (assoc table-sym (tables-of tune))) value))

(defun tune-add-table (tune table-sym)
  (push (cons table-sym (make-array '(0) :initial-element nil
				    :adjustable t))
	(tables-of tune)))

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
