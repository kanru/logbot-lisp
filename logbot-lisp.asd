(in-package #:cl-user)

(asdf:defsystem logbot
  :version "0.0.1"
  :author "Kan-Ru Chen <kanru@kanru.info>"
  :licence "MIT/Expat"
  :description "IRC Logbot"
  :depends-on ("cl-irc2" "sqlite" "usocket")
  :serial t
  :components ((:file "packages")
               (:file "db")
               (:file "irc")
               (:file "web")
               (:file "runner")))
