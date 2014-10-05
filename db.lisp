;;;; db.lisp --- Logbot Database

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

(defun initialize-database (pathname channels)
  (sqlite:with-open-database (db pathname)
    (sqlite:execute-non-query
     db "CREATE TABLE IF NOT EXISTS channels (
           id INTEGER PRIMARY KEY,
           name TEXT NOT NULL
         );")
    (sqlite:execute-non-query
     db "CREATE TABLE IF NOT EXISTS irclogs (
           id INTEGER PRIMARY KEY,
           channel_id INTEGER,
           datetime INTEGER,
           nick TEXT,
           action TEXT,
           msg TEXT,
           FOREIGN KEY(channel_id) REFERENCES channels(id)
         );")
    (loop :for channel :in channels
          :do (sqlite:execute-non-query
               db "INSERT OR FAIL INTO channels (name) VALUES (?);"
               channel))))

(defun database-open (pathname)
  (sqlite:connect pathname))

(defun database-close (db)
  (sqlite:disconnect db))

(defun log-message (db channel nick action msg)
  (let ((datetime (get-universal-time)))
    (sqlite:execute-non-query
     db "INSERT INTO irclogs (channel_id, datetime, nick, action, msg)
         VALUES ((SELECT id FROM channels WHERE name=?),?,?,?,?);"
     channel datetime nick action msg)))

(defun query-message (db channel start end)
  (sqlite:execute-to-list
   db "SELECT datetime, nick, action, msg FROM irclogs
       WHERE datetime >= ? AND datetime <= ? AND
       channel_id = (SELECT id FROM channels WHERE name=?);"
   start end channel))

;;; db.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
