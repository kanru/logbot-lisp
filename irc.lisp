;;;; irc.lisp --- Logbot IRC client

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

(defclass logbot (irc:client)
  ((%db :initarg :database
        :accessor logbot-db)))

(defun irc-logbot (pathname server port nick channels)
  (let* ((usocket (usocket:socket-connect server port))
         (stream (usocket:socket-stream usocket))
         (conn (irc:make-connection stream))
         (db (database-open pathname))
         (client (make-instance 'logbot
                                :nick nick
                                :user "logbot"
                                :realname "Logbot-Lisp"
                                :channels channels
                                :database db)))
    (unwind-protect
         (irc:with-connection (conn)
           (irc:connect-run-main-loop client))
      (database-close db)
      (usocket:socket-close usocket))))

(defun ctcp-action-p (msg)
  (when msg
    (let ((code001 (code-char 1)))
      (and (char= code001 (char msg 0))
           (char= code001 (char msg (1- (length msg))))
           (= 7 (mismatch "ACTION " msg :start2 1))))))

(defun ctcp-action-message (msg)
  (when msg
    (let ((start (length " ACTION "))
          (end (1- (length msg))))
      (subseq msg start end))))

(defun try-sandbox-repl (privmsg)
  (let ((chl (first (irc-message:message-args privmsg)))
        (msg (second (irc-message:message-args privmsg)))
        (nik (getf (irc-message:message-prefix privmsg) :nickname)))
    (let ((sandbox:*sandbox* (format nil "SANDBOX/~a/~a" chl nik)))
      (with-output-to-string (datum)
        (handler-case
            (bt:with-timeout (1)
              (sandbox:read-eval-print (subseq msg 1) datum))
          (bt:timeout () (write-line ";; TIMEOUT" datum)))
        (irc:send-message 'irc-message:privmsg chl
                          (string-trim '(#\Newline)
                                       (get-output-stream-string datum))))))

(defmethod irc:handle-message ((client logbot) (privmsg irc-message:privmsg))
  (let* ((msg (second (irc-message:message-args privmsg)))
         (chl (first (irc-message:message-args privmsg)))
         (nik (getf (irc-message:message-prefix privmsg) :nickname))
         (ctcp-action-p (ctcp-action-p msg))
         (ctcp-message (if ctcp-action-p (ctcp-action-message msg) nil)))
    (log-message (logbot-db client) chl nik
                 (if ctcp-action-p "ACTION" "PRIVMSG")
                 (if ctcp-action-p ctcp-message msg))
    (when (and (not ctcp-action-p)
               (string= "," msg :end2 1))
      (log-message (logbot-db client) chl (irc:client-nick client)
                   (try-sandbox-repl privmsg)))))

;;; irc.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
