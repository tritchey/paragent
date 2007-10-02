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

(defcomponent preferences-tabbed-view (tabbed-view)
  ((user :accessor user
          :initarg :user
          :type user)
   (message :accessor message
	    :initarg :message
            :type string
	    :initform "")
   (users-list :accessor users-list
               :type user-list)
   (group-list :accessor group-list
               :type group-list)
   (ticket-email-smtp :accessor ticket-email-smtp
		      :type ticket-email-smtp)
   (ticket-email-list :accessor ticket-email-list
                      :type ticket-email-list))
  (:default-initargs
    :tabs (list "Reports")))


(defgeneric render-company-info (page))

(defgeneric render-users-tab (page))

(defgeneric render-groups-tab (page))

(defgeneric render-tickets-tab (page))

(defmethod initialize-instance :after ((page preferences-tabbed-view) &key title)
  (declare (ignore title))
  (setf (toolbar page) #'render-company-info)
  (setf (message page) "")
  (let ((user (user page)))
    (when (>=admin user)
      (setf (users-list page) (make-instance 'user-list :user user :parent page))
      (setf (group-list page) (make-instance 'group-list :user user :parent page))
      (setf (ticket-email-smtp page) (make-instance 'ticket-email-smtp :user user :parent page))
      (setf (ticket-email-list page) (make-instance 'ticket-email-list :user user :parent page))
      (setf (contents page)
	    (list #'render-users-tab
		  #'render-groups-tab
		  #'render-tickets-tab))
      (setf (tabs page)
	    (list "Users" "Groups" "Tickets"))
      ;; normally this is handled in the tabbed-view :after,
      ;; but since we are resetting the number of tabs here
      ;; we need to run it again
      (initialize-ids page))))

(defmethod render-company-info ((page preferences-tabbed-view))
  (<:h2 "Company settings " (<:ah (case (level (company (user page)))
                                        (#.+account-free+ "(Free Account)")
                                        (#.+account-basic+ "(Basic Account)")
                                        (#.+account-plus+ "(Plus Account)")
                                        (#.+account-premium+ "(Premium Account)"))))
  (when (not-blank (message page))
    (render-tip-box (<:ah (message page))))
  (setf (message page) ""))

(defmethod render-user-info ((page paragent-component))
  (let* ((user (user page))
         (password1 "")
         (password2 password1)
         (admin (>= (level user) 10))
         (checkbox (make-instance 'checkbox-field :value (weekly-software-report user))))
    (<:div :class "box-title"
           (<:h2 "Your user account: " (<:as-html (username user))))
    (<:div 
     :class "box"
     (unless (equal "" (message page))
       (<:div  :id "system-note"
               (<:p (<:as-html (message page))))
       (setf (message page) ""))
     (<ucw:form
      :action (save-user-changes page user password1 password2 checkbox) :id "own-info-form"
      (<:table
       :id "alerts"
       :class "item-list"
       :cellspacing 5
       (<:tr
        (<:td :class "title"
              (<:ah "Full Name"))
        (<:td :class "desc"
              (<ucw:text :class "full-name" :accessor (name user))))
       (<:tr
        (<:td :class "title"
              (<:ah "Change Password"))
        (<:td :class "desc"
              (<ucw:password :class "passwd" :accessor password1)))
       (<:tr
        (<:td :class "title"
              (<:ah "Confirm Password"))
        (<:td :class "desc"
              (<ucw:password :class "passwd" :accessor password2)))
       (<:tr
        (<:td :class "title"
              (<:ah "Weekly Reports"))
        (<:td :class "desc"
	      (<:span
	       (<:label :for (dom-id checkbox) :class "software-pref"
			(render checkbox)
			(<:strong " Software ")
			(<:ah "- All the software that has been added to or removed from your network in the past week.")))))
       (<:tr
        (<:td :class "title"
              (<:as-is "&nbsp;"))
        (<:td :class "desc"
              (<:a :href "#"
		   :onclick "javascript:$('own-info-form').submit(); return false;"
		   (<:img :src "/images/savebtn.gif")))))))))

(defmethod render-users-tab ((page preferences-tabbed-view))
  (render (users-list page)))

(defmethod render-groups-tab ((page preferences-tabbed-view))
  (render (group-list page)))

(defmethod render-tickets-tab ((page preferences-tabbed-view))
  (<:div 
   :class "tab-page-section"
   (<:h2 "SMTP Server information")
   (<:p "This is the username and server we will use to send replies to tickets for this account")
   (render (ticket-email-smtp page)))
  (<:div
   :class "tab-page-section"
   (<:h2 "Ticket Email Accounts")
   (<:p "These POP3 email accounts will be checked for incoming tickets. " 
	(grey-box "Watch this email address..." "Add email account for tickets..."
		  (goto-ticket-email-dialog page) 240 360))
   (render (ticket-email-list page))))



(defaction goto-preferences-page ((page simple-window-component) user)
  (call 'preferences-page :user user))

(defcomponent preferences-page (paragent-window-component)
  ((tab-view :accessor tab-view
	    :initarg :tab-view
	    :type preferences-tabbed-view))
  (:default-initargs
    :title "Setup"))

(defmethod initialize-instance :after ((page preferences-page) &key)
  (setf (tab-view page) (make-instance 'preferences-tabbed-view :user (user page))))


(defrender ((page preferences-page))
  (init-grey-box)
  (unless (equal "" (message page))
    (<:div  :id "system-note"
            (<:p (<:as-html (message page))))
    (setf (message page) ""))
  (let ((user (user page)))
    (if (guest-p user)
        (show-tip user
                  "On this page, a person would be able to change their password and personal preferences.
                  Administrators can also add and remove users. This functionality has been disabled for this demo.")
        (progn
          (let ((msi (msi (company user))))
            (when msi
              (render-tip-box 
               (<:ah "Install ") (<:a :href msi "this program") (<:ah " on more computers to monitor them."))))
          (render-user-info page)
	  (when (>=admin user)
	    (render (tab-view page)))))))

(defmethod render-tasks ((page preferences-page))
  (when (>= (level (user page)) 10)
    (<:h1 "Tasks")
    (<:ul
     (<:li (grey-box "New User..." "Add new user..." (new-user page) 380 450))
     (<:li (grey-box "New Group..." "Add new group..." (new-group page) 450 500))
     (<:li (grey-box "Watch this email address..." "Add email account for tickets..."
                     (goto-ticket-email-dialog page) 240 360)))))

(defvar +ascii-alphabet+
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890")

(defun random-password (&optional (length 8) (alphabet +ascii-alphabet+))
  (let ((rs (make-random-state t)))
    (loop with id = (make-string length)
          with alphabet-length = (length alphabet)
          for i below length
          do (setf (cl:aref id i)
                   (cl:aref alphabet (random alphabet-length rs)))
          finally (return id))))







#.(clsql:restore-sql-reader-syntax-state)
