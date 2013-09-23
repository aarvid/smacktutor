;;;; smacktutor.asd

(asdf:defsystem #:smacktutor
  :serial t
  :version "0.1"
  :description "Allows for tutorials to run in the smacklisp interpreter"
  :author "Andy Peterson <andy.arvid@gmail.com>"
  :license "MIT"
  :depends-on (#:smacklisp)
  :components ((:file "package")
               (:file "smacktutor")
               (:file "tutorial-tutorial")
               (:file "tutorial-repl")
               (:file "tutorial-newbie")))

