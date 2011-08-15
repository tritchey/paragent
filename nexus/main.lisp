#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

(in-package :nexus)

#.(clsql:locally-enable-sql-reader-syntax)

(defaction goto-front-page ((page paragent-window-component) user)
  (call 'front-page :user user))

(defaction goto-front-page ((page paragent-container) user)
  (call 'front-page :user user))

(defcomponent front-page (paragent-window-component)
  ((event-list :accessor event-list
               :type inline-event-list))
  (:documentation "The main dashboard. This is the first thing the user sees.")
  (:default-initargs 
   :title "Your network"))

(defmethod body-id ((component front-page))
  "dashboard")


(defmethod initialize-instance :after ((page front-page) &key)
  (setf (event-list page)
        (make-instance 'inline-event-list :user (user page) :condensed t)))

(defrender ((page front-page))
  (let* ((user (user page))
         (computer-count (db-count 'computers :where [= [company-id] (company-id user)]))
         (today (get-date))
         (event-count (db-count 'events :where [and [= [company-id] (company-id user)] [>= [timestamp] today]]))
         (software-count (db-count [software] :where [= [company-id] (company-id user)]))
         (msi (msi (company user))))
    (with-db
      (cond
        ((<= computer-count 0)
         (render-tip-box
           (<:h3 "Where are my computers?")
           (<:p "You must install the Paragent Agent on your computers in order to monitor them.")
           (if msi
               (unless (equal msi "")
                 (<:p "You can download the Agent " (<:a :href msi "here") "."))
               (<:p "A custom installer is currently being generated for you. You will receive an email with instructions on where to download it soon, or you can simply check back here in a few minutes."))))
        ((and (<= computer-count 10) msi)
         (render-tip-box
           (<:p "Install the " (<:a :href msi "Paragent Agent") " on more computers to monitor them!")))))
    (unless (equal *message* "")
      (<:div :id "system-note"
             (<:p (<:as-html *message*))))
    (when (guest-p user)
      (<:div :class "tip" :id "status-tip"
             (<:a :class "close-button" :href "#"
                  :onclick "javascript:Effect.Fade($('status-tip')); return false;"
                  (<:img :src "images/close.gif"))
             (<:ah "The status box gives you a quick summary of your network health.")))
    (show-tip user
              "Below you can see the most recent events on your network, such as when a computer is turned on, what software has been recently installed, any alerts you have created, etc.")
    (let ((num-logged-errors (db-count [logged-errors] :where [= [company-id] (company-id user)])))
      (when (and (>=admin user) (> num-logged-errors 0))
        (render-tip-box
         (<:ah "Errors have occurred. " (<ucw:a :action (goto-logged-error-page page) "Click here")
               " to view them."))))
    (let ((clients (mapcar #'car (with-db (select 'client :where [= [company-id] (company-id user)])))))
      (<:div :class "box-title"
	     (<:h2 "Events"))
      (<:div
       :class "box"
       (<:table
	:class "dashboard"
	(<:tr (<:td (<wormtrail-chart page clients)))
	(<:tr
	 (<:td :class "both" :colspan "2"
	       (<:h2 "Clients" (<:a ))
	       (dolist (client clients)
		 (let* ((name (name client))
			(online-count 
			  (with-db (db-count 'computers :where [and [= [client-id] (id client)] [= [online] 1]])))
			(offline-count 
			  (with-db (db-count 'computers :where [and [= [client-id] (id client)] [= [online] 0]])))
			(ticket-count 
			  (with-db (db-count 'tickets :where [= [client-id] (id client)])))
			(event-count 
			 (caar
			  (with-db (query (format nil "select count(events.id) from events join computers on events.computer_id = computers.id where computers.client_id=~a" (id client))))))
			(display-name (if (> (length name) 17)
					  (format nil "~a&#0133;" (subseq name 0 15))
					  name)))
		 (<ucw:a :class "client-box-anchor" :action (goto-client page (id client) name)
			 (<:div :class "client-box"
				(<:h3 (<:as-is display-name) (<:span :id "offline-count" (<:as-html online-count) "/" (<:as-html offline-count)))
				(<:ul 
				 (<:li :id "online-count" (<:as-html online-count))
				 (<:li :id "event-count" :class (if (>= event-count 10) "warning" "") (<:as-html event-count))
				 (<:li :id "ticket-count" :class (if (>= ticket-count 10) "warning" "") (<:as-html ticket-count))))))))))))))



#.(clsql:restore-sql-reader-syntax-state)


