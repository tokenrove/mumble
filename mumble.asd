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
	       (:file "music-utilities" :depends-on ("classes"))
	       (:file "music-parser" :depends-on ("classes"
						  "music-utilities"))
	       (:file "mumble" :depends-on ("music-parser"))
	       (:file "replay-ymamoto" :depends-on ("mumble"))
	       (:file "replay-xm" :depends-on ("mumble"))))))

