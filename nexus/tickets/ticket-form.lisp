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



;;; the ticket form component

(defcomponent ticket-form (paragent-component)
  ((ticket :accessor ticket
           :initarg :ticket
           :type ticket)
   (internalp :accessor internalp
	      :initarg :internalp
	      :type boolean
	      :initform t)
   (computer-ids :accessor computer-ids
                 :initarg :computer-ids
                 :type list
                 :initform nil)
   (assigned-user-field :accessor assigned-user-field)
   (state-field :accessor state-field
                :type alist-select-field
                :initform (make-instance
                            'alist-select-field
                            :test-fn #'equal
                            :data-set '(("Closed" . 0)
                                        ("Open" . 1)
                                        ("On Hold" . 2))))
   (priority-field :accessor priority-field
                   :type alist-select-field
                   :initform (make-instance
                               'alist-select-field
                               :test-fn #'equal
                               :data-set '(("Note" . 0)
                                           ("Low" . 1)
                                           ("Medium" . 2)
                                           ("High" . 3)
                                           ("Critical" . 4))))
   ; TODO this needs to have a time component
   (due-date-field :accessor due-date-field
                   :type calendar-date-field
                   :initform (make-instance 'calendar-date-field))
   (response-email-field :accessor response-email-field
                         :type string-field
                         :initform (make-instance 'string-field :css-class "emails" :value ""))
   (subject-field :accessor subject-field
                  :type string-field
                  :initform (make-instance 'string-field :css-class "subject" ))
   (description-field :accessor description-field
                      :type textarea-field
                      :initform (make-instance 'textarea-field :cols 60 :rows 5))
   (tags-field :accessor tags-field
               :type string-field
               :initform (make-instance 'string-field))
   (reason-field :accessor reason-field
                 :type textarea-field
                 :initform (make-instance 'textarea-field :rows 3)
                 :documentation "Used when making changes to a ticket to describe the reason for the change")))

(defmethod initialize-instance :after ((form ticket-form) &key)
  (with-db
    (let ((ticket (ticket form)))
      (setf (ucw::events (tags-field form))
            `(("onfocus" (actb ($ ,(dom-id (tags-field form)))
                               (list ,@(get-categories (company-id ticket)))))))
      (setf (assigned-user-field form)
            (progn
              (make-instance
                'alist-select-field
                :test-fn #'equal
                :value (when (assigned-user ticket) (username (assigned-user ticket)))
                :data-set
                (cons
                  (cons "(Unassigned)" nil)
                  (mapcar
                    (lambda (user)
                      (cons (username user) (id user)))
                    (select 'user :flatp t :order-by [username]
                            :where [= [company-id] (company-id (user form))]))))))
      
      (setf (value (subject-field form)) (subject ticket))
      (setf (value (description-field form)) (body ticket))
      (setf (value (tags-field form)) (tags-to-string (tags ticket)))
      (when (due-date ticket)
        (setf (value (due-date-field form)) (due-date ticket)))
      (setf (value (priority-field form)) (priority-string ticket))
      (setf (value (state-field form)) (state-string ticket))
      (if (response-email ticket)
          (setf (value (response-email-field form)) (response-email ticket))
          (setf (value (response-email-field form)) (email (user form)))))))

(defmethod render ((form ticket-form))
  (let* ((ticket (ticket form)))
    (<:script :src "actb/actb.js" :type "text/javascript")
    (<:script :src "actb/common.js" :type "text/javascript")
    (<ucw:form
      :action (save-ticket form)
      (unless (new-p form)
        (<:p 
          (<:label "Reason for this change:")
          (<:span (render (reason-field form)))))
      (<:p :class "basic-input"
        (<:label "Subject")
        (<:span (render (subject-field form))))
      (<:p :class "label-on-top"
        (<:label "Description")
        (<:span (render (description-field form))))
      (<:p :class "basic-input"
        (<:label "Response e-mail")
        (<:span (render (response-email-field form))))
      (when (internalp form)
	 (<:p :class "due-date"
	  (<:label "Due Date")
	  (<:span (render (due-date-field form))))
	 (<:p  :class "basic-input"
	  (<:label "Categories")
	  (<:span (render (tags-field form))))
	 (<:p
	  (<:label "Assigned User")
	  (<:span (render (assigned-user-field form))))
	 (<:p
	  (<:label "Priority")
	  (<:span (render (priority-field form))))
	 (<:p
	  (<:label "Status")
	  (<:span (render (state-field form)))))
      (<:p
        (<ucw:input :action (save-ticket form)
                    :class "save"
                    :type "image"
                    :src "images/savebtn.gif")))))

(defaction save-ticket ((form ticket-form))
  (when (save-ticket% form)
    (if (new-p form)
        (goto-dialog-confirm form "Ticket saved." (ticket-link (ticket form)))
        (call-component nil (make-instance 'dialog-confirm 
					   :header "Ticket Saved"
					   :message "The changes to your ticket have been saved."
                                           :redirect-to (ticket-link (ticket form)))))))


(defmethod new-p ((form ticket-form))
  (new-p (ticket form)))
    
(defmethod new-p ((ticket ticket))
  (not (ticket-id ticket)))


(defgeneric save-ticket% (form))

(defmethod save-ticket% ((form ticket-form))
  (with-db
    (when (validp form)
      (let* ((ticket (ticket form))
             (assigned-user-id (value (assigned-user-field form)))
             (old-assigned-user-id (assigned-user-id ticket))
             (state (value (state-field form)))
             (priority (value (priority-field form)))
             (due-date (value-as-time (due-date-field form)))
             (subject (value (subject-field form)))
             (body (value (description-field form)))
             (response-email (value (response-email-field form)))
             (reason (value (reason-field form)))
             (tags (value (tags-field form))))
        (macrolet ((update-diff (var change-desc &optional (comparer 'equal))
                     `(let ((old (,var ticket))
                            (new ,var))
                        (when (and (not (new-p ticket)) (not (,comparer old new)))
                          (update-records-from-instance
                           (make-instance 'ticket-change :ticket-id (id ticket)
                                          :user-id (id (user form))
                                          :note reason
                                          :description ,change-desc)))
                        (setf (,var ticket) ,var))))
          (update-diff assigned-user-id (format nil "Ticket assigned to ~a (was previously ~a)."
                                                (if new
                                                    (username (car (select 'user :flatp t
                                                                           :limit 1
                                                                           :where [= [id] new])))
                                                    "nobody")
                                                (if (assigned-user ticket)
                                                    (username (assigned-user ticket))
                                                    "unassigned")))
          (update-diff state (status-change-description old new))
          (update-diff priority (format nil "Priority changed from ~a to ~a." (priority-string old) (priority-string new)))
          (update-diff due-date (format nil "Due date changed from ~a to ~a."
                                        (or old "None") (or new "None"))
                       (lambda (a b) (or (and (equal a nil) (equal b nil)) (and a b (time= a b)))))
          (update-diff subject (format nil "Subject changed from '~a' to '~a.'" old new))
          (update-diff body (format nil "Description changed from '~a' to '~a.'" old new))
          (update-diff response-email (format nil "Response email changed from ~a to ~a." old new)))
        
        (unless (ticket-id ticket)
          (setf (ticket-id ticket) (get-next-ticket-id (company-id (user form)))))
        (insert-and-update ticket)
        (when (and assigned-user-id (or (new-p form) (not (equal assigned-user-id old-assigned-user-id))))
          (let ((assigned-user (car (select 'user :flatp t :limit 1 :where [= [id] assigned-user-id]))))
            (send-ticketing-email (user form) (list (email assigned-user))
                                  (format nil "You have been assigned Ticket ~a - ~a" (ticket-id ticket) subject)
                                  (format nil "A ticket has been assigned to you. Go to ~a~a to view it."
                                          *server-url* (ticket-link ticket)))))
        (let ((old-tags (tags-to-string (tags ticket))))
          (unless tags (setf tags ""))
          (when (not (equal old-tags tags))
            (save-tags ticket tags)
            (update-records-from-instance
             (flet ((format-tags (tags)
                      (if (or (not tags) (equal tags ""))
                          "Uncategorized"
                          (format nil "'~a'" tags))))
               (make-instance 'ticket-change :ticket-id (id ticket)
                              :user-id (id (user form))
                              :note reason
                              :description (format nil "Categories changed from ~a to ~a."
                                                   (format-tags old-tags)
                                                   (format-tags tags)))))))
        (dolist (computer-id (computer-ids form))
          (update-records-from-instance
           (make-instance 'ticket-computer :ticket-id (id ticket) :computer-id computer-id))))
      t)))

(defun tags-to-string (tags)
  (let ((ret ""))
    (dolist (tag tags)
      (setf ret (concatenate 'string ret (name tag) " ")))
    ret))

(defmethod save-tags ((ticket ticket) tag-string)
  (with-db
    (clsql:with-transaction
      ()
      (clsql:delete-records :from [ticket-tags] :where [= [ticket-id] (id ticket)])
      (dolist (tag (remove-duplicates 
		    (split-sequence:split-sequence-if (lambda (c) (or (equal c #\space) (equal c #\+)))
						      tag-string
						      :remove-empty-subseqs t)))
        (slot-makunbound ticket 'tags)
        (clsql:update-records-from-instance
          (make-instance 'ticket-tag
                         :ticket-id (id ticket)
                         :name tag))))))

#.(clsql:restore-sql-reader-syntax-state)
