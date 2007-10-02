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

;;; User list

(defcomponent user-list (vertical-tabbed-view)
  ((parent :accessor parent
           :initarg :parent)
   (users :accessor users
          :type list
	  :initform ())
   (message :accessor message
            :initform ""))
  (:default-initargs
      :toolbar #'render-title))

(defmethod initialize-instance :after ((page user-list) &key title)
  (declare (ignore title))
  (with-db
    (let ((users (unoptimized-list-getter page)))
      (setf (users page) users)
      (setf (contents page)
	    (mapcar #'(lambda (user) 
			#'(lambda (page) (render-item page user)))
		    users))
      (initialize-ids page))))

(defmethod render :before ((page user-list))
  "We reset the tab titles each render in case the name of the item got changed"
  (setf (tabs page)
        (mapcar #'(lambda (user) 
                    (let ((name (name user))
                          (email (username user)))
                      (if (not (equal "" name))
                          name
                          email)))
                (users page))))
  

(defmethod unoptimized-list-getter ((page user-list))
  (let ((user (user page)))
    (remove-if
      (let ((username (username user)))
        (lambda (other-user)
          (equal (username other-user) username)))
      (if (>=admin user)
          (select 'user :order-by [username] :flatp t
                  :where [= [company-id] (company-id user)])
          '()))))

(defmethod render-title ((page user-list))
   (<:h2 "Users in your company"))
  
  
(defmethod render-item ((page user-list) user)
  (<:div 
   :class "user-info"
   (let* ((admin (>=admin (user page)))
	  (groups (when admin
		    (let ((user-groups (groups user)))
		      (remove-if
		       (lambda (group+checkbox)
			 (default-group-p (car group+checkbox)))
		       (mapcar
			(lambda (group)
			  (cons group
				(make-instance 'checkbox-field :value (find group user-groups :test #'name=))))
			(groups (company-id (user page))))))))
	  (id (unique-id "user-form")))
     
     (<ucw:form
      :action (save-user-changes page user nil nil nil groups) 
      :id id
      (<:table 
       :id "alerts" :class "item-list"
       (<:tr
	(<:td :class "title"
	      (<:ah "Email Address"))
	(<:td :class "desc"
	      (<:ah (email user))))
       (<:tr
	(<:td :class "title"
	      (<:ah "Full Name"))
	(<:td :class "desc"
	      (<ucw:text :class "full-name" :accessor (name user))))
       (when admin
	 (<:tr
	  (<:td :class "title"
		(<:ah "Account type:"))
	  (<:td :class "desc" (render-user-level user))))
       (<:tr
	(<:td :class "title"
	      (<:ah "Groups"))
	(<:td :class "desc"
	      (<:ul
	       (dolist (group groups)
		 (let ((checkbox (cdr group))
		       (group (car group)))
		   (<:li
		    (<:label :for (dom-id checkbox)
			     (render checkbox)
			     (<:ah " " (name group)))))
		 groups))))
       (<:tr
	(<:td :class "title" (<:as-is "&nbsp;"))
	(<:td :class "desc"
	      (when admin
		(<:ul
		 :class "action-list"
		 (<:li :class "first-item"
		       (<:a
			:href "#"
			:onclick (format nil "javascript:$('~a').submit(); return false;" id)
			"save"))
		 (<:li
		  (<ucw:a :action (reset-user-password page user) "reset password"))
		 (<:li
		  (<ucw:a :action (delete-user page user)
			  :onclick "return confirm('Delete this user?');"
			  "delete")))))))))))


(defun render-user-level (user)
  (<ucw:select :accessor (level user)
               (<ucw:option :value 0 "User")
               (<ucw:option :value 10 "Administrator")))


;;; New user page


(defcomponent new-user-page (paragent-dialog-component)
  ((message :initarg :message
            :accessor message
            :initform "")
   (new-user :initarg :new-user
             :type user)))

(defmethod initialize-instance :after ((page new-user-page) &key)
  (setf (slot-value page 'new-user) (make-instance 'user :company-id (company-id (user page)))))

(defmethod render ((page new-user-page))
  (let ((user (slot-value page 'new-user)))
    (let* ((password1 "")
           (password2 password1))
        (<:fieldset
          :class "box user-form"
          (<ucw:form
            :action (save-new-user page user password1 password2)
            (<:p :class "error" (<:as-html (or (message page) (<:as-is "&nbsp;"))))
            (<:p
              (<:label "Email:")
              (<ucw:text :accessor (username user)))
            (<:p
              (<:label "Account type:")
              (render-user-level user))
            (<:p
              (<:label "Full Name:")
              (<ucw:text :accessor (name user)))
            (<:p
              (<:label "Password:")
              (<ucw:password :accessor password1))
            (<:p
              (<:label "Confirm Password:")
              (<ucw:password :accessor password2))
            (<:p
              (<ucw:input :action (save-new-user page user password1 password2)
                          :class "save"
                          :type "image"
                          :src "images/savebtn.gif")))))))


;;; Actions

(defaction save-new-user ((page new-user-page) user password password-confirm)
  (when (%save-new-user page user password password-confirm)
    (call 'dialog-confirm :user (user page) :message "User created" :redirect-to *url-prefs*)))

(defun %save-new-user (page user password password-confirm)
  (with-db
    (cond
      ((not (equal password password-confirm))
       (setf (message page) "The passwords you entered do not match")
       nil)
      ((equal password "")
       (setf (message page) "You must enter a password")
       nil)
      ((select 'user :where [= [username] (username user)])
       (setf (message page) "That username is already taken. Please choose another one.")
       nil)
      ((equal (username user) "")
       (setf (message page) "You must enter a username")
       nil)
      ((or (not (find #\@ (username user))) (not (find #\. (username user))))
       (setf (message page) "You must provide a valid email address")
       nil)
      ((find #\" (username user))
       (setf (message page) "Your username may not include a quote")
       nil)
      ((find #\" password)
       (setf (message page) "Your password may not include a quote")
       nil)
      (t (progn
           (when (and password (not (equal password "")))
             (setf (password user) (hash-password (username user) password)))
           (setf (email user) (username user))
           (clsql:update-records-from-instance user)
           t)))))


(defun save-record (record)
  (with-db
    (clsql:update-records-from-instance record)))

(defaction new-user ((page paragent-window-component))
  (call 'new-user-page :user (user page)))

(defaction delete-user ((page paragent-component) user)
  (%delete-user page user)
  (call-component nil (make-instance 'preferences-page :user (user page))))

(defun %delete-user (page user)
  (declare (ignore page))
  (with-db
    (clsql:delete-instance-records user)))


(defaction save-user-changes ((page paragent-component) user password password-confirm &optional checkbox group+checkboxes)
  (save-user-changes% page user password password-confirm checkbox group+checkboxes))

(defgeneric save-user-changes% (page user password password-confirm &optional checkbox group+checkboxes))

(defmethod save-user-changes% ((page paragent-component) user password password-confirm &optional checkbox group+checkboxes)
  (if (equal password password-confirm)
      (with-db
        (when (and password (not (equal password "")))
          (let ((pass-hash (hash-password (username user) password)))
            (setf (password user) pass-hash)))
        (setf (message page) "")
        (when checkbox
          (setf (weekly-software-report user) (value checkbox)))
        (insert-and-update user)
        (when group+checkboxes
          (delete-records :from (table-for-object 'group-user-link)
                          :where [= [slot-value 'group-user-link 'user-id] (id user)])
          (dolist (group+checkbox group+checkboxes)
            (let ((group (car group+checkbox)))
              (when (value (cdr group+checkbox))
                (update-records-from-instance
                  (make-instance 'group-user-link :group-id (id group) :user-id (id user))))))))
      (progn
        (setf (message page) "The passwords you entered did not match"))))



(defaction reset-user-password ((page paragent-component) user)
  (setf (message (parent page)) (reset-user-password% user)))

(defmethod reset-user-password% ((user user))
  (let ((password (random-password)))
    (with-db
      (setf (password user) (hash-password (username user) password))
      (update-records-from-instance user))
    (unless (send-email (list (email user)) "Paragent Password"
                        (format nil
                                "Your Paragent password has been changed to '~a'. After logging in, you can change it at https://archon.paragent.com/preferences.ucw"
                                password))
      (format nil "Paragent was unable to email the new password to ~a. That password is '~a'."
              (username user)
              password))))




#.(clsql:restore-sql-reader-syntax-state)
