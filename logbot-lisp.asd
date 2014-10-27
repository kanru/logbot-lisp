(in-package #:cl-user)

(asdf:defsystem logbot-lisp
  :version "0.0.1"
  :author "Kan-Ru Chen <kanru@kanru.info>"
  :licence "MIT/Expat"
  :description "IRC Logbot"
  :depends-on ("cl-irc2"
               "bordeaux-threads"
               "sqlite"
               "usocket"
               "hunchentoot"
               "cl-mustache"
               "local-time"
               "lass")
  :serial t
  :components ((:file "packages")
               (:file "db")
               (:file "irc")
               (:file "web")
               (:file "runner")))
