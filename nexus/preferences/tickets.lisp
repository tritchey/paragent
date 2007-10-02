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


(defcomponent ticket-email-smtp (paragent-component)
  ((email-smtp :accessor email-smtp
	       :initarg :email-smtp
	       :type db::email-smtp)
   (authp :accessor authp
	  :initarg authp
	  :initform (make-instance 'checkbox-field)
	  :type checkbox-field)))

(defmethod initialize-instance :after ((page ticket-email-smtp) &key)
  (with-db
   (let* ((user (user page))
          (email-smtp (db::email-smtp (company-id user))))
     (setf (email-smtp page) email-smtp))))

(defmethod render ((page ticket-email-smtp))
  (let ((email-smtp (email-smtp page)))
  (let ((id (unique-id "email-smtp-form")))
    (<:div
     (<ucw:form
      :action (save-email-smtp page email-smtp) :id id
      (setf (password email-smtp) "")
      (tableify
       ((<:label "Reply-to: ")
	(<ucw:text :accessor (db::reply-to email-smtp)))
       ((<:label "Host: ")
	(<ucw:text :accessor (host email-smtp)))
       ((render (authp page))
	(<:label "My Server Requires Authentication"))
       ((<:label "Username: ")
	(<ucw:text :accessor (username email-smtp)))
       ((<:label "Password: ")
	(<ucw:password :accessor (password email-smtp))))
      (<:ul
       :class "action-list"
       (<:li :class "first-item"
             (<:a :href "#"
                  :onclick (format nil "javascript:$('~a').submit(); return false;" id)
                  "save"))))))))

(defcomponent ticket-email-list (vertical-tabbed-view)
  ((parent :accessor parent
           :initarg :parent)
   (ticket-emails :accessor ticket-emails
                  :type list))
  (:default-initargs
    :toolbar #'render-title))

(defmethod initialize-instance :after ((page ticket-email-list) &key)
  (with-db
   (let* ((user (user page))
          (ticket-emails (ticket-emails (company-id user))))
     (setf (ticket-emails page) ticket-emails)
     (setf (contents page)
           (mapcar (lambda (ticket-email)
                     (lambda (page) (render-item page ticket-email)))
                   ticket-emails))
     (initialize-ids page))))

(defmethod render :before ((page ticket-email-list))
  "We reset the tab titles each render in case the name of the item got changed"
  (setf (tabs page)
        (mapcar #'username (ticket-emails page))))

(defmethod render-title ((page ticket-email-list))
  (<:h2 "Current Accounts:"))


(defmethod unoptimized-list-getter ((page ticket-email-list))
  (let ((user (user page)))
    (select 'ticket-email :flatp t
            :where [= [company-id] (company-id user)])))

(defmethod render-item ((page ticket-email-list) ticket-email)
  (let ((id (unique-id "ticket-email-form")))
    (<:div
     (<ucw:form
      :action (save-ticket-email page ticket-email) :id id
      (render (make-instance 'ticket-email-form :user (user page) :ticket-email ticket-email))
      (<:ul
       :class "action-list"
       (<:li :class "first-item"
             (<:a :href "#"
                  :onclick (format nil "javascript:$('~a').submit(); return false;" id)
                  "save")))))))


;; Form

(defcomponent ticket-email-form (paragent-component)
  ((ticket-email :accessor ticket-email
                 :initarg :ticket-email
                 :type ticket-email)))

(defmethod render ((form ticket-email-form))
  (let ((ticket-email (ticket-email form)))
    (setf (password ticket-email) "")
    (tableify
      ((<:label "Host: ")
       (<ucw:text :accessor (host ticket-email)))
      ((<:label "Port: ")
       (<ucw:text :accessor (port ticket-email)))
      ((<:label "Username: ")
       (<ucw:text :accessor (username ticket-email)))
      ((<:label "Password: ")
       (<ucw:password :accessor (password ticket-email))))))


;; Dialog

(defaction goto-ticket-email-dialog ((page paragent-component))
  (call 'ticket-email-dialog :user (user page)))

(defcomponent ticket-email-dialog (paragent-dialog-component)
  ())

(defmethod render ((page ticket-email-dialog))
  (let* ((user (user page))
         (ticket-email (make-instance 'ticket-email :company-id (company-id user)))
         (form (make-instance 'ticket-email-form :user user :ticket-email ticket-email)))
    (<:fieldset
      :class "ticket-email-form"
      (<ucw:form
        :action (save-ticket-email page ticket-email)
        (render form)
        (<ucw:input :action (save-ticket-email form ticket-email)
                    :type "image" :class "save"
                    :value "Save" :src "images/savebtn.gif")))))


;; Methods for my madness

(defaction save-ticket-email ((page paragent-component) ticket-email)
  (save-ticket-email%  ticket-email))

(defaction save-ticket-email ((page ticket-email-dialog) ticket-email)
  (save-ticket-email%  ticket-email)
  (call-component nil (make-instance 'dialog-confirm :message "Account added"
                                     :redirect-to *url-prefs*)))

(defun save-ticket-email% (ticket-email)
  (setf (port ticket-email) (or (parse-integer (port ticket-email) :junk-allowed t) 110))
  (with-db
    (update-records-from-instance ticket-email)))

(defaction save-email-smtp ((page ticket-email-smtp) email-smtp)
  (setf (authp email-smtp) (value (authp page)))
  (save-email-smtp%  email-smtp))

(defun save-email-smtp% (email-smtp)
  (with-db
      (update-records-from-instance email-smtp)))


#.(clsql:restore-sql-reader-syntax-state)
