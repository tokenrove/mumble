;;;
;;; Code to support outputting simple FastTracker XMs from mumble.
;;;
;;; Julian Squires <tek@wiw.org> / 2004
;;;

(in-package :mumble)

;;; converting:
;;;
;;; default to 1/16th note per row, read in all note data, allocating
;;; patterns accordingly. (have temporary patterns, containing rows
;;; with simultaneous notes together)
;;;
;;; go back through patterns; when strange/fast timings occur, try to
;;; frob tempo appropriately to accomidate.  finally, pack patterns.
;;;
;;; load instruments from XI
