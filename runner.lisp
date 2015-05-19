;;;; runner.lisp --- Logbot RUNNER

;;; Copyright (C) 2014  Kan-Ru Chen (陳侃如)

;;; Author(s): Kan-Ru Chen (陳侃如) <kanru@kanru.info>

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;;;; Commentary:

;;; 

;;;; Code:

(in-package #:logbot)

(defparameter *database* #P"irclog.sqlite")
(defparameter *nick* "bibot")
(defparameter *server* "irc.freenode.net")
(defparameter *port* 6667)
(defparameter *channels* '("#h4" "#emacs.tw" "#lisp.tw" "#cschat.tw"))
(defparameter *webserver-port* 8000)
(defvar *bot-thread*)

(defun start-bot ()
  (initialize-database *database* *channels*)
  (run-webserver *webserver-port*)
  (setf *bot-thread*
        (bt:make-thread
         (lambda ()
           (irc-logbot *database* *server* *port* *nick* *channels*))
         :name "Logbot-IRC")))

(defun stop-bot ()
  (bt:destroy-thread *bot-thread*)
  (stop-webserver))


;;; runner.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
