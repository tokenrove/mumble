;; -*- Lisp -*-

(defpackage #:mumble-system (:use #:cl #:asdf))
(in-package #:mumble-system)

(defsystem mumble
    :depends-on (:anaphora)
    :components
    ((:module :src
	      :components
	      ((:file "package")
	       (:file "classes" :depends-on ("package"))
	       (:file "music-parser" :depends-on ("classes"
						  "music-utilities"))
	       (:file "music-utilities" :depends-on ("classes"))
	       (:file "replay-ymamoto" :depends-on ("music-parser"))
	       (:file "replay-xm" :depends-on ("music-parser"))))))

