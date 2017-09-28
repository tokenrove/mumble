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

(defparameter *staccato-base-division* 1/8)
(defvar *radix* 10)
(defconstant +octave-size+ 12)

;;;; LOW-LEVEL PARSE/LEX ROUTINES.

(defun expect-natural (stream)
  (do ((i nil (let ((d (digit-char-p (read-char stream))))
               (if i (+ d (* i *radix*)) d))))
      ((not (digit-char-p (peek-char nil stream nil #\a))) i)))

(defun expect-duration (stream)
  (let* ((duration (make-duration (expect-natural stream)))
         ;; if the next character is a dot, read dots until the next
         ;; character is not a dot.
         (dots (loop while (char= #\. (peek-char nil stream))
                     for i from 1
                     sum (/ duration (ash 1 i))
                     do (read-char stream))))

    (when duration (incf duration dots))

    ;; tie.
    (when (and duration (char= #\^ (peek-char nil stream)))
      (read-char stream)
      (incf duration (expect-duration stream)))
    duration))

(defun expect-accidentals (stream)
  (loop until (char/= (peek-char nil stream) #\+ #\-)
        sum (if (char= (read-char stream) #\+) 1 -1)))

(defun expect-note (stream)
  (let* ((note-char (read-char stream))
         (accidentals (expect-accidentals stream))
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
  (loop for next-char = (peek-char nil stream)
        until (char= next-char #\})
        collect (cond ((char= next-char #\|)
                       (read-char stream)
                       :loop)
                      ((find next-char "0123456789-") (read stream)))
        do (eat-whitespace stream)
        finally (assert (char= #\} (read-char stream)))))

(defun read-symbols-macro (stream)
  (assert (char= (read-char stream) #\{))
  (loop for symbol = (read stream)
        until (eql symbol '})
        collect symbol))

(defparameter *macro-table-mapping*
  '((#\a :arpeggio read-numbers-and-loop-macro)
    (#\v :volume-envelope read-numbers-and-loop-macro)
    (#\i :instrument read-symbols-macro)
    (#\~ :vibrato read-symbols-macro)))

(defun read-macro-definition (stream)
  (let* ((dispatch (read-char stream))
         (index (expect-natural stream))
         (mapping (find dispatch *macro-table-mapping* :test #'equal :key #'first)))
    (expect-= stream)
    (values (second mapping) index (funcall (third mapping) stream))))


(defun handle-simple-volume (stream channels)
  (assert (char= (read-char stream) #\v))
  (let ((fn (ecase (peek-char nil stream)
              ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
               (constantly (expect-natural stream)))
              (#\+ (read-char stream) #'1+)
              (#\- (read-char stream) #'1-))))
    (dolist (c channels)
      (let ((v (funcall fn (volume-of c))))
        (vector-push-extend (make-volume-command v) (data-stream-of c))
        (setf (volume-of c) v)))))

(defun clarify-duration (duration channel)
  (if duration
      (setf (default-duration-of channel) duration)
      (default-duration-of channel)))

(defun calculate-tone (char accidentals octave)
  (+ (* +octave-size+ octave)
     (position char *note-characters*)
     accidentals))

;;;; HIGH-LEVEL PARSE ROUTINES.

;;; We should really just create a readtable for the use of all the
;;; following routines.  Basically, what's in parse-header-section,
;;; but with the other CL standard macro characters disabled (parens,
;;; single/back quote, comma).
(defun parse-mumble-file (stream tune)
  (let ((*read-eval* nil)
        (*package* (find-package :mumble)))
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
           (push (funcall (replay-channel-creator (replay-of tune)))
                 (tracks-of tune)))
          ((TITLE COMPOSER COPYRIGHT)
           (push (list header argument) (metadata-of tune))))))))


(defun parse-macro-section (stream tune)
  (eat-whitespace stream)
  (loop for next-char = (read-char stream)
        do (cond ((char= next-char #\@)
                  (multiple-value-bind (table index entry)
                      (read-macro-definition stream)
                    (assert (plusp index) ()
                            "Bad index ~A (tables index from 1 -- 0 is the ~
                     \"effect off\" index)." index)
                    (unless (tune-get-table tune table)
                      (tune-add-table tune table))
                    (tune-add-to-table tune table index entry)))

                 ;; Section change.
                 ((char= next-char #\#) (return))
                 ;; Comment.
                 ((char= next-char #\;) (read-line stream))
                 ;; Something else?
                 (t (format t "~&Ignored character in macro section: ~A (~:*~S)"
                            next-char)))
           (eat-whitespace stream)))

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
             (assert (< c (length (channels-of-current-track-of tune)))
                     () "Invalid channel for this replay.")
             (push (nth c (channels-of-current-track-of tune)) current-channels)))

          ;; Repeats (unrolled loops).
          ((char= next-char #\[)
           (read-char stream)
           (dolist (c current-channels)
             (push (channel-current-position c) (repeats-of c)))
           (parse-music-section stream tune current-channels t))

          ((char= next-char #\])
           (assert (and in-loop-p current-channels))
           (read-char stream)
           (let ((count (expect-natural stream)))
             (dolist (c current-channels)
               (let ((begin (pop (repeats-of c)))
                     (end (1- (channel-current-position c))))
                 (dotimes (i (1- count))
                   (copy-and-append-channel-data c begin end)))))
           (return))

          ;; Octave changes.
          ((find next-char #(#\o #\< #\>))
           (read-char stream)
           (let ((f (ecase next-char
                      (#\o (constantly (expect-natural stream)))
                      (#\< #'1-) (#\> #'1+))))
             (dolist (c current-channels)
               (setf (octave-of c) (funcall f (octave-of c))))))

          ;; (Non-venv) volume changes.
          ((char= next-char #\v) (handle-simple-volume stream current-channels))

          ;; Notes and rests.
          ((find next-char *note-characters*)
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
           (multiple-value-bind (note-type duration) (expect-rest stream)
             (dolist (c current-channels)
               (vector-push-extend (make-note note-type (clarify-duration duration c))
                                   (data-stream-of c)))))

          ;; Tempo or staccato change.
          ((find next-char #(#\t #\q))
           (read-char stream)
           (let* ((value (expect-natural stream))
                  (command (make-instance 'music-command :type (case next-char (#\t :tempo) (#\q :staccato)) :value value)))
             (dolist (c current-channels)
               (vector-push-extend command (data-stream-of c))
               (ecase next-char
                 (#\t (setf (tempo-of c) value))
                 (#\q (setf (staccato-of c) value))))))

          ;; Section change.
          ((char= next-char #\#)
           (read-char stream)
           (when in-loop-p
             (warn "Changing sections during a [] repeat."))
           (return))

          ;; Macro invocation.
          ((char= next-char #\@)
           (parse-macro-invocation stream current-channels))

          ;; Structural dispatch character.
          ((char= next-char #\!)
           (parse-bang-invocation stream current-channels))

          ;; Replay-special invocation.
          ((char= next-char #\%)
           (read-char stream)
           (funcall (replay-special-handler (replay-of tune))
                    stream current-channels))

          ;; Comment.
          ((char= next-char #\;) (read-line stream))

          ;; Something else?
          (t (format t "~&Ignored character in music section: ~A (~:*~S)"
                     (read-char stream))))
    (eat-whitespace stream *ws-and-barline-characters*)))


(defun parse-macro-invocation (stream channels)
  (read-char stream)
  (let* ((next-char (read-char stream))
         (n (expect-natural stream))
         (type (second (find next-char *macro-table-mapping* :test #'equal :key #'first))))
    (dolist (c channels)
      (vector-push-extend (make-instance 'music-command :value n :type type)
                          (data-stream-of c)))))

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
