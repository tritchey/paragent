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



;;; Dialog to add new tickets


(defaction new-ticket ((page paragent-component))
  (call 'edit-ticket-dialog :user (user page)))

(defaction edit-ticket ((page paragent-component) ticket &key computer-ids)
  (call-component nil (make-instance 'edit-ticket-dialog :user (user page)
                                     :ticket ticket :computer-ids computer-ids)))



(defcomponent edit-ticket-dialog (paragent-dialog-component)
  ((ticket :accessor ticket
           :type ticket
           :initarg :ticket)
   (ticket-form :accessor ticket-form
                :type ticket-form))
  (:default-initargs
    :title "Edit Ticket"))

(defun make-default-ticket (user)
  (make-instance 'ticket :user-id (id user) :company-id (company-id user)
                 :state 1 :priority 2 :assigned-user-id (id user) :assigned-user user
                 :response-email (email user)))
  
(defmethod initialize-instance :after ((page edit-ticket-dialog) &key computer-ids)
  (with-db
    (let* ((user (user page))
           (ticket (if (slot-boundp page 'ticket)
                       (ticket page)
                       (make-default-ticket user))))
      (setf (ticket page) ticket)
      (setf (ticket-form page) (make-instance 'ticket-form :ticket ticket :user user :computer-ids computer-ids)))))

(defmethod render ((page edit-ticket-dialog))
  
  (<:fieldset
    :id "new-ticket" :class "box"
    (render (ticket-form page))))



;;; Dialog to add a comment to a ticket

(defaction close-ticket ((page paragent-component) ticket)
  (call-component nil (make-instance 'add-ticket-comment-page :user (user page) :ticket ticket :title "Close ticket"))
  (close-ticket% page ticket)
  (goto-dialog-confirm page "Ticket closed." *url-tickets*))

(defun close-ticket% (page ticket)
  (with-db
   (update-records-from-instance
    (make-instance 'ticket-change :ticket-id (id ticket)
                   :user-id (id (user page))
                   :description (format nil (status-change-description (state ticket) +ticket-status-closed+))))
   (setf (state ticket) +ticket-status-closed+)
   (update-records-from-instance ticket)))

(defaction add-comment-to-ticket ((page paragent-component) ticket)
  (call-component nil (make-instance 'add-ticket-comment-page :user (user page) :ticket ticket))
  (goto-dialog-confirm page "Comment added." (ticket-link ticket)))

(defcomponent add-ticket-comment-page (paragent-dialog-component)
  ((ticket :accessor ticket
           :initarg :ticket
           :type ticket))
  (:default-initargs
    :title "Comment"))

(defmethod render ((page add-ticket-comment-page))
  (let ((comment "") 
        (ticket (ticket page)))
    (<:fieldset :id "ticket-comment" :class "box"
      (<ucw:form
        :action (do-add-comment page ticket comment)
        (<:h3 (<:ah "This comment " (<:i "will not") " be emailed"))
        (<:p
          (<ucw:textarea :accessor comment :rows 10 :style "width:98%;"))
        (<:p
          (<ucw:submit :action (do-add-comment page ticket comment)
		       :class "login"
		       :type "image"
		       :src "images/savebtn.gif"))))))

(defaction do-add-comment ((page add-ticket-comment-page) ticket comment)
  (do-add-comment% page ticket comment)
  (answer t))

(defun do-add-comment% (page ticket comment)
  (declare (string comment))
  (declare (ticket ticket))
  (with-db
    (update-records-from-instance
      (make-instance 'ticket-comment :ticket-id (id ticket) :body comment :user-id (id (user page))))))


;;; Dialog to reply to a ticket

(defaction add-reply-to-ticket ((page paragent-component) ticket)
  (call-component nil (make-instance 'add-ticket-reply-page :user (user page) :ticket ticket :parent-page (parent page))))

(defcomponent add-ticket-reply-page (paragent-dialog-component)
  ((ticket :accessor ticket
           :type ticket
           :initarg :ticket)
   (parent-page :accessor parent-page
		:initarg :parent-page
		:initform nil))
  (:documentation "Dialog to reply to a ticket")
  (:default-initargs
    :title "New Ticket"))

(defmethod render ((page add-ticket-reply-page))
  (let ((comment "") 
        (ticket (ticket page)))
    (<:fieldset :id "ticket-reply" :class "box"
      (<ucw:form
        :action (do-add-reply page ticket comment)
        (<:h3 (<:ah "This reply " (<:i "will") " be emailed to " (response-email ticket)))
        (<:p
          (<ucw:textarea :accessor comment :rows 10 :style "width:98%;"))
        (<:p
          (<ucw:submit :action (do-add-reply page ticket comment)
		       :class "login"
		       :type "image"
		       :src "images/replybtn.gif"))))))

(defaction do-add-reply ((page add-ticket-reply-page) ticket reply)
  (if (do-add-reply% page ticket reply)
      (goto-dialog-confirm page "Reply added." (ticket-link ticket))
      (goto-dialog-alert page "There appears to be a problem with the email account we have on record for ticket responses, and we were unable to send the ticket using this account." (ticket-link ticket))))

(defun do-add-reply% (page ticket reply)
  (declare (string reply))
  (declare (ticket ticket))
  (with-db
    (if (not-blank (response-email ticket))
        (send-ticketing-email (user page) (list (response-email ticket))
                              (subject-for ticket) reply)
        t)))

(defun subject-for (ticket)
  (format nil "[Case ~a] ~a" (ticket-id ticket) (subject ticket)))




;; Dialog to associate a computer with a ticket

(defaction open-computer-ticket-dialog ((page paragent-component) ticket)
  (call-component nil (make-instance 'computer-ticket-dialog :ticket ticket :user (user page))))

(defcomponent computer-ticket-dialog (paragent-dialog-component)
  ((ticket :accessor ticket
           :initarg :ticket
           :type ticket)
   (computer-selector :accessor computer-selector
                      :type computer-selector))
  (:default-initargs
    :title "Add/Remove Computers"))

(defmethod initialize-instance :after ((page computer-ticket-dialog) &key)
  (with-db
    (setf (computer-selector page)
          (make-instance 'computer-selector :selected (computers (ticket page)) :user (user page)))))

(defmethod render ((page computer-ticket-dialog))
  (with-db
    (<:script :src "prototype.js" :type "text/javascript")
    (init-computer-selector (user page))
    (<ucw:form
      :action (set-computers page)
     (<:fieldset
      :id "add-remove-computer"
      :class "box"
        (render (computer-selector page))
        (<ucw:submit :action (set-computers page) 
		     :class "login"
		     :type "image"
		     :src "images/savebtn.gif")))))
  

(defaction set-computers ((page computer-ticket-dialog))
  (set-computers% (computer-selector page) (ticket page))
  (goto-dialog-confirm page "Computers added." (ticket-link (ticket page))))


(defun set-computers% (computer-selector ticket)
  (with-db
    (delete-records :from (table-for-object 'ticket-computer)
                    :where [= [slot-value 'ticket-computer 'ticket-id] (id ticket)])
    (let ((selected-ids (new-selection computer-selector)))
      (dolist (computer-id selected-ids)
        (update-records-from-instance
          (make-instance 'ticket-computer :ticket-id (id ticket) :computer-id computer-id))))))

;;; Externally visible page for others to submit tickets

(defcomponent external-ticket-form (ticket-form)
  ()
  (:documentation "Allows anybody on the internet to submit a ticket")
  (:default-initargs
      :internalp nil))

(defaction save-ticket ((form external-ticket-form))
  (when (save-ticket% form)
    (goto-message-page form "Your ticket has been submitted.")))

(defun send-ticket-submitted-email (form)
  (let ((response-email (value (response-email-field form))))
    (when (not-blank response-email)
      (send-email (list response-email) "Your ticket has been submitted"
                  "Your ticket has been received, and will be reviewed by a technician shortly."))))
       

(defcomponent external-ticket-page (paragent-window-component)
  ((company :accessor company
            :initarg :company
            :type company)
   (company-name :accessor company-name
                 :type string
                 :initarg :company-name)
   (ticket-form :accessor ticket-form
                :type external-ticket-form))
  (:default-initargs :title "Submit ticket"
    :stylesheet "css/ticket.css"))

(defmethod initialize-instance :after ((page external-ticket-page) &key)
  (let ((company (company page)))
    (when company
      (setf (ticket-form page)
            (make-instance 'external-ticket-form 
			   :ticket (make-instance 'ticket :company-id (id company)
							  :state 1 
							  :priority 2 
							  :assigned-user-id nil)
                           :user (make-instance 'user :company-id (id company)) 
			   :company company)))))

(defmethod render ((page external-ticket-page))
  (if (company page)
      (<:div :class "submit-ticket"
	(<:div
	 :id "submit-ticket"
	 :class "box-title"
	 (<:h2 "Submit trouble ticket to " (<:ah (company-name page))))
	(<:fieldset :id "new-ticket" :class "box"
	       (render (ticket-form page))))
      (progn
        (<:h3 "There is no company named " (<:ah (company-name page))))))

#.(clsql:restore-sql-reader-syntax-state)
