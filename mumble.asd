;; -*- Lisp -*-

(in-package :asdf-user)

(defsystem mumble
  :depends-on ("anaphora" "alexandria")
  :pathname "src/"
  :components
  ((:file "package")
   (:file "classes" :depends-on ("package"))
   (:file "music-utilities" :depends-on ("classes"))
   (:file "music-parser" :depends-on ("classes"
                                      "music-utilities"))
   (:file "mumble" :depends-on ("music-parser"))
   (:file "replay-ymamoto" :depends-on ("mumble"))
   (:file "replay-xm" :depends-on ("mumble")))
  :in-order-to ((test-op (test-op "mumble/tests"))))

(defsystem "mumble/tests"
  :depends-on ("mumble" "fiveam")
  :pathname "tests/"
  :serial t
  :components ((:file "package"))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :mumble)))
