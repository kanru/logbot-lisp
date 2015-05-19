;;;; web.lisp --- LogBot web viewer

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

(defvar *webserver-acceptor*)
(defvar *irclog-database*)
(defparameter *default-index-view* "/channel/emacs.tw/today")

(defun index-view ()
  (hunchentoot:redirect *default-index-view*))

(defun channel-view ()
  (let ((scanner (ppcre:create-scanner "/channel/([^/]+)(?:/([^/]+))?/?$"
                                       :single-line-mode t)))
    (lambda ()
      (let ((url (hunchentoot:script-name*)))
        (multiple-value-bind (start end match-start match-end)
            (ppcre:scan scanner url)
          (declare (ignore end))
          (flet ((match (n)
                   (when (elt match-start n)
                     (subseq url (elt match-start n) (elt match-end n)))))
            (if (null start)
                (hunchentoot:redirect *default-index-view*)
                (let ((channel (match 0))
                      (date (match 1)))
                  (if (null date)
                      (hunchentoot:redirect (format nil "/channel/~a/today" channel))
                      (channel-date-view (format nil "#~a" channel) date))))))))))

(defun format-start-date-time (ut)
  (let ((timestamp (local-time:universal-to-timestamp ut)))
    (local-time:format-timestring nil timestamp
                                  :format '(:year "-" :month "-" :day " "
                                            (:hour 2) ":" (:min 2) ":" (:sec 2) "~"))))
(defun format-time (ut)
  (let ((timestamp (local-time:universal-to-timestamp ut)))
    (local-time:format-timestring nil timestamp
                                  :format '((:hour 2) ":" (:min 2) ":" (:sec 2)))))

(defun date-universal-time-start-end (date-string)
  (let ((ut (cond
              ((string= date-string "today")
               (local-time:timestamp-to-universal (local-time:today)))
              ((string= date-string "yesterday")
               (local-time:timestamp-to-universal
                (local-time:timestamp- (local-time:today) 1 :day)))
              (t
               (handler-case
                   (local-time:timestamp-to-universal
                    (local-time:parse-timestring date-string))
                 (error ()
                   (local-time:timestamp-to-universal (local-time:today))))))))
    (values (- ut 86400)
            (+ ut 86400))))

(defun channel-date-view (channel date)
  (multiple-value-bind (start end)
      (date-universal-time-start-end date)
    (let ((messages (query-message *irclog-database* channel start end)))
      (setf (hunchentoot:content-type*) "text/html")
      (with-output-to-string (mustache:*output-stream*)
          (render-channel-date-view `((:channel . ,channel)
                                      (:start-date-time . ,(format-start-date-time start))
                                      (:messages . ,(messages-context messages))))))))

(defun messages-context (messages)
  (loop :for (datetime nick action msg) :in messages
        :collect `((:time . ,(format-time datetime))
                   (:nick . ,(if (string= action "ACTION") "*" nick))
                   (:action . ,action)
                   (:msg . ,(if (string= action "ACTION")
                                (format nil "~a ~a" nick msg)
                                msg)))))

(mustache:define render-channel-date-view
  "<!DOCTYPE html>
<html>
  <head>
    <title>Logbot | {{channel}}</title>
    <link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/screen.css\">
    <link rel=\"styleshee\" type=\"text/css\" href=\"http://fonts.googleapis.com/css?family=Ropa+Sans\">
    <link rel=\"styleshee\" type=\"text/css\" href=\"http://fonts.googleapis.com/css?family=Droid+Sans+Mono\">
    <base target=\"_blank\" />
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\"/>
    <script>
      window.setTimeout(function() { location.reload(); }, 60000);
    </script>
  </head>
  <body>
    <div class=\"wrapper\">
      <div class=\"header\">
        <h1>Logbot</h1>
      </div>
      <!--
      <div class=\"quick-nav\">
        <div class=\"nav_page-up\" onclick=\"pageScrollTop(0);\">⬆</div>
        <div class=\"nav_page-down\" onclick=\"pageScrollTop($(document).height());\">⬇</div>
      </div>
      -->
      <div class=\"body\">
        <div class=\"channel\">{{channel}}</div>
        <div class=\"date\">{{start-date-time}}</div>
        
        <!--<div class=\"scroll_switch\">AUTO⬇ </div>-->
       
        <div>
          <ul class=\"logs\">
            {{#messages}}
              <li class=\"type-{{action}}\">
                <span class=\"time\">{{time}}</span>
                <span class=\"nick\">{{nick}}</span>
                <span class=\"msg wordwrap\">{{msg}}</span>
              </li>
            {{/messages}}
          </ul>
        </div>
      </div>
      <div class=\"footer\">
        <p>Powered by Logbot-Lisp 2014 | <a href=\"https://github.com/kanru/logbot-lisp\">GitHub Repo</a></p>
      </div>
    </div>
    <!--<script src=\"http://code.jquery.com/jquery-1.8.3.min.js\"></script>-->
    <script>
      var channel = \"{{channel}}\";
    </script>
    <!--<script src=\"/assets/applications.js\"></script>-->
  </body>
</html>
")

(defun screen-css-view ()
  (setf (hunchentoot:content-type*) "text/css")
  (setf (hunchentoot:header-out :expires)
        (hunchentoot:rfc-1123-date
         (+ (get-universal-time) 120)))
  (setf (hunchentoot:header-out :cache-control) "max-age=120")
  (let ((base03 "#002b36")
        (base02 "#073642")
        (base01 "#586e75")
        (base00 "#657b83")
        (base0 "#839496")
        (base1 "#93a1a1")
        (base2 "#eee8d5")
        (base3 "#fdf6e3")
        (yellow "#b58900")
        (orange "#cb4b16")
        (red "#dc322f")
        (magenta "#d33682")
        (violet "#6c71c4")
        (blue "#268bd2")
        (cyan "#2aa198")
        (green "#859900")
        (row-color-odd "rgba(0,0,0,.33)")
        (row-color-even "rgba(0,0,0,.15)"))
    (lass:compile-and-write
     `(:keyframes blink
       (from :background-color ,yellow)
       (to   :background-color transparent))
     `(html
       (* :color-profile sRGB
          :rendering-intent auto))
     `(body
       :background ,base02
       :color ,base00
       :font-family "Helvetica" sans-serif
       (a :color ,blue
          :text-decoration none)
       ((:and a :hover) :background ,blue
                        :color ,base03))
     `(.wrapper :width 90%
                :margin 0 auto)
     `(.header
       :padding 30px 0
       :text-align center
       (h1
        :display inline-block
        :font-size 32pt
        :font-family "Ropa Sans" sans-serif
        :color ,base02
        :font-weight bold
        :background ,green
        :padding 5px 20px
        :text-transform uppercase))
     `(.footer
       :padding 40px 0 30px 0
       :font-family "Ropa Sans" sans-serif
       :text-transform uppercase
       :text-align center)
     `(body
       ((:or .channel .date .scroll_switch)
        :color ,cyan
        :font 14pt/20px "Droid Sans Mono" monospace
        :display inline-block
        :border-radius 5px
        :cursor pointer)
       ((:and (:or .channel .date .scroll_switch) :hover) :background-color ,base03)
       (.channel
        :font-weight bold
        :padding 5px 10px)
       (.date
        :background-size 10%
        :color ,violet
        :border none
        :padding 5px 10px
        :outline none
        :appearance none)
       ((:and .date "::after")
        :content ,(format nil "'~c'" (code-char #x25BE)))
       (.scroll_switch
        :float right
        :clear both
        :padding 5px 10px
        :color ,green)
       (.scroll_switch_off
        :color ,green
        :text-decoration line-through)
       (.logs
        :list-style-type none
        :font-family "Droid Sans Mono" monospace
        :margin 0 10px
        (li ((:or .time .nick)
             :float left)
            ((:or .time .nick .msg)
             :display block)
            (.time
             :width 70px
             :vertical-align text-top
             :letter-spacing -2px)
            (.nick
             :width 110px
             :color ,yellow
             :text-align right
             :letter-spacing -1px
             :margin-left 5px)
            (.msg
             :color ,base1
             :margin-left 200px
             (.nick
              :width auto
              :margin-left 0px))
            :margin-top 12px
            :line-height 1.33)
        ((:and li .new-arrival)
         :animation blank 500ms ease-out)))
     `(.wordwrap
       :word-break break-all
       :hypens auto)
     `(.quick-nav
       :position fixed
       :top 45%
       :right 3%
       :display inline-block
       :text-align center)
     `((:or .nav_page-up .nav_page-down)
       :background-color ,base03
       :color ,base1
       :font-size 14pt
       :padding 5px
       :border-radius 5px)
     `((:and (:or .nav_page-up .nav_page-down) :hover)
       :cursor pointer
       :background-color ,base01
       :color ,base03)
     `(.nav_page-down
       :margin-top 15px)
     `(:media "only screen and (max-width: 568px)"
              (.body
               (.logs
                (li
                 :padding 10px
                 (.nick :width auto)
                 (.msg :margin-left 0
                       :clear left))
                ((:and li (:nth-child odd))
                 :background ,row-color-odd)
                ((:and li (:nth-child even))
                 :background ,row-color-even)))
              ((:or .nav_page-up .nav_page-down)
               :padding 1px)))))

(defun run-webserver (port)
  (setf hunchentoot:*dispatch-table*
        (list (hunchentoot:create-regex-dispatcher "^$" #'index-view)
              (hunchentoot:create-regex-dispatcher "^/$" #'index-view)
              (hunchentoot:create-regex-dispatcher "^/channel/" (channel-view))
              (hunchentoot:create-regex-dispatcher "^/assets/screen.css$" #'screen-css-view))
        *webserver-acceptor*
        (make-instance 'hunchentoot:easy-acceptor :address "127.0.0.1" :port port)
        *irclog-database*
        (database-open *database*))
  (hunchentoot:start *webserver-acceptor*))

(defun stop-webserver ()
  (hunchentoot:stop *webserver-acceptor*)
  (database-close *irclog-database*))

;;; web.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
