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
    (<:div :class "box-title"
	   (<:h2 "Summary"))
    (<:div
     :class "box"
    (<:table
     :class "dashboard"
     (<:tr
      (<:td :class "both" :colspan "2"
            (<:p
              (<:ah (<:strong (<:ah event-count)) " Events today"))
	    (<:p
	      (<:ah (<:strong (<:ah computer-count)) " Computers"))
	    (<:p
	      (<:ah (<:strong (<:ah software-count)) " Installed Software Packages"))))
     (<:tr
      (<:td
       :class "left chart"
       (<:h2 "Events")
       (<event-timeline-graph page user :height 100 :width 250))
      (<:td
	:class "right chart"
       (<:h2 "Online status")
       (let ((offline-count (db-count 'computers :where [and [= [company-id] (company-id user)]
                                      [not [= [online] 1]]])))
         (<pie-chart page `((,(chart::get-color :medium-circle)
                              "Offline"
                              ,(if (> computer-count 0)
                                   (/ offline-count computer-count)
                                   1))
                              (,(chart::get-color :note-circle)
                                "Online"
                                ,(if (> computer-count 0)
                                     (/ (- computer-count offline-count) computer-count)
                                     0)))
                       :radius 50))))
     (<:tr
      (<:td :class "both" :colspan "2"
	    (render (event-list page))))
     (let ((active-comps (select [slot-value 'computer 'name] [count [slot-value 'event 'id]]
				 :from (list [computers] [events])
				 :where [and
                                 [= [slot-value 'computer 'company-id] (company-id user)]
                                 [= [slot-value 'computer 'id] [slot-value 'event 'computer-id]]]
				 :group-by [slot-value 'computer 'name]
				 :order-by (list (list [count [slot-value 'event 'id]] :desc))
				 :limit 5))
	   (active-alerts (select [slot-value 'alert 'id] [slot-value 'alert 'type-id] [slot-value 'alert 'args]
				  [count [slot-value 'event 'id]]
				  :from (list [alerts] [alert-event-link] [events])
				  :where [and
	                          [= [slot-value 'alert 'company-id] (company-id user)]
	                          [= [slot-value 'alert 'id] [slot-value 'alert-event-link 'alert-id]]
	                          [= [slot-value 'alert-event-link 'event-id] [slot-value 'event 'id]]]
				 :group-by [slot-value 'alert 'id]
				 :order-by (list (list [count [slot-value 'event 'id]] :desc))
				 :limit 5)))
       (<:tr
        (<:td
         :class "left last"
         (<:h2 "Most active computers")
         (<:table
          :width "100%" :border 0 :class "aligner top-ten"
          (dolist (computer+events active-comps)
            (<:tr
             (<:td :align "left" :class "aligner"
                   (<:img :width "24px" 
                          :height "24px"
                          :src "images/computer.gif")
                   " "
                   (<:a :href (computer-link (first computer+events))
                        (<:ah (first computer+events))))
             (<:td :align "right" :class "aligner"
                   (<:ah (second computer+events)))))))
        (<:td
         :class "right last"
         (<:h2 "Most active alerts")
         (<:table
          :width "100%" :border 0 :class "aligner top-ten"
          (dolist (info active-alerts)
            (let ((alert (make-instance 'alert :id (first info) :type-id (second info)
                                        :args (clsql-sys:read-sql-value 
                                               (third info)
                                               t 
                                               *default-database* 
                                               (database-type *default-database*)))))
              (<:tr
               (<:td :align "left" :class "aligner"
                     (render-icon alert) " "
                     (<:a :href (alert-link alert)
                          (<:ah (description alert))))
               (<:td :align "right" :class "aligner"
                     (<:ah (fourth info))))))))))))))



#.(clsql:restore-sql-reader-syntax-state)


