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

(defcomponent login-page (simple-window-component)
  ((msg :initarg :msg
        :initform ""
        :type string
        :accessor msg)
   (username :initarg :username
             :initform ""
             :type string
             :accessor username)
   (remember-me :accessor remember-me
                :type checkbox-field
                :initform (make-instance 'checkbox-field :value t)))
  (:default-initargs :title "Log in"
   :stylesheet "css/login.css"))


(defmethod render ((page login-page))
  (<:script :src "prototype.js")
  (when (string= (username page) "") (setf (username page) (get-cookie "user")))
  (<:div :id "login"
	 (<:div :class "box-title"
		(<:h2 "Paragent.com Sign In"))
	 (<:fieldset :class "box"
		(let ((password ""))
		  (<ucw:form
		   :method "post" :action (do-login page password)
		   (when (plusp (length (msg page)))
		     (<:p :class "error"
			  (<:as-html 
			   (msg page) 
			   (<:a :href "mailto:support@paragent.com" "support") ".")))
		   (<:p
		    (<:label "Email:")
		    (<ucw:input :id "username"
				:class "full-name"
				:name "username"
				:type "text"
				:accessor (username page)))
		   (<:p
		    (<:label "Password:")
		    (<ucw:input :id "password"
				:class "passwd"
				:name "password"
				:type "password"
				:accessor password))
		   (<:p
		    :id "remember-me"
		    (render (remember-me page))
		    (<:label :for (dom-id (remember-me page)) " Remember me"))
		   (left-right-align
		    (<ucw:input
		     :action (do-login page password)
		     :value "login"
		     :class "login"
		     :type "image"
		     :src "images/login.gif")
		    (<ucw:input
		     :class "reset-password"
		     :type "image"
		     :src "images/resetpasswdbtn.gif"
		     :action (reset-user-password page (username page))
		     :value "Reset Password"))))))
	 (<:script "$('username').focus();"))


(defaction reset-user-password ((page login-page) user-name)
  (if (reset-user-password% user-name)
      (setf (msg page) "An email will be sent with your new password. If you have any questions, please contact us at ")
      (setf (msg page) (format nil "The email address '~a' does not exist in our database. Please contact " user-name))))

(defgeneric reset-user-password% (user-name))

(defmethod reset-user-password% ((user-name string))
  (with-db
    (let ((user (car (select 'user :flatp t :limit 1
                             :where [= [username] user-name]))))
      (if user
          (progn
            (reset-user-password% user)
            t)
          nil))))
  
(defaction do-login ((page login-page) password)
  (let* ((username (username page))
         (valid-user (check-login username password)))
    (if valid-user
        (if (disabled valid-user)
            (setf (msg page) "We are sorry, it appears your account has been disabled. If you believe you have reached this message in error, please contact us at ")
            (progn
              (save-login-time username)
              (call 'login-redirector :username username :password password :remember-me (value (remember-me page)))))
        (setf (msg page) "Your login information was incorrect. Either we do not have the email address in our database, or the password does not match the email address provided. If you believe you have reached this message in error, please contact us at "))))

(defgeneric save-logon-time (user)
  (:documentation "Tries to save when the user last logged in. Doesn't work too well"))

(defmethod save-login-time ((username string))
  (with-db
    (let ((user (car (select 'user :flatp t :limit 1 :where [= [username] username]))))
      (when user
        (setf (last-login user) (clsql:get-time))
        (clsql:update-records-from-instance user)))))

(defmethod save-login-time ((user user))
  (with-db
    (when user
      (setf (last-login user) (clsql:get-time))
      (clsql:update-records-from-instance user))))

(defun check-login (username password)
  (with-db
    (let ((user (car (select 'user :flatp t :limit 1 :where [= [username] username]))))
      (when (and user (string= (password user) (hash-password username password)))
        user))))


;; Login page that sends you back to the entry point you wanted

(defcomponent redirecting-login-page (login-page)
  ()
  (:documentation "One of the few places where we really take advantage of UCW's niftiness.
Call this page to login, and then use the returned user to go on to the page you wanted."))

(defaction do-login ((page redirecting-login-page) password)
  (let* ((username (username page))
         (valid-user (check-login username password)))
    (if valid-user
        (if (disabled valid-user)
            (setf (msg page) "We are sorry, it appears your account has been disabled. If you believe you have reached this message in error, please contact us at ")
            (progn
              (save-login-time username)
              (make-user-session valid-user (value (remember-me page)))
              (answer valid-user)))
        (setf (msg page) "Your login information was incorrect. Either we do not have the email address in our database, or the password does not match the email address provided. If you believe you have reached this message in error, please contact us at "))))


(defun make-user-session (user remember-me)
  (declare (type user user))
  (declare (type boolean remember-me))
  (with-db
   (let ((session-id (generate-session-id)))
     (clsql:with-transaction
      ()
      (do-while (select 'user-session :where [= [session-id] session-id] :flatp t :limit 1)
        (setf session-id (generate-session-id)))
      (let ((session (if remember-me
                         (make-instance 'user-session :session-id session-id :user-id (id user))
                         (make-instance 'user-session :session-id session-id :user-id (id user)
                                        :expiration (time+ (get-time) (make-duration :hour 5)))))
            (expiration (if remember-me
                            (* 60 60 24 7 2)
                            nil)))
        (update-records-from-instance session)
        (set-cookie "user" (username user) expiration)
        (set-cookie "session-id" session-id expiration))))))


(defcomponent login-redirector (simple-window-component)
  ((username :initarg :username
             :type string
             :initform ""
             :accessor username)
   (password :initarg :password
             :type string
             :initform ""
             :accessor password)
   (remember-me :accessor remember-me
                :initarg :remember-me
                :initform nil
                :type boolean))
  (:documentation "Called by the login page to set cookies and redirect to the main page.")
  (:default-initargs
    :stylesheet "css/login.css"))

(defmethod render ((page login-redirector))
  (let* ((username (username page))
         (password (password page))
         (valid-user (check-login username password)))
    (when valid-user
      (make-user-session valid-user (remember-me page)))
    (<:meta :http-equiv "refresh"
            :content (format nil "0;url=~a" *url-main*))
    (<:div :id "login"
    (<:div :class "box-title" 
	   (<:h2  "Logging you in..."))
    (<:div :class "box"
	   (<:img :style "margin: 0 109px 0 89px"
		  :width "32px"
		  :height "32px;"
		  :src "/images/ajax-spinner.gif")))))


(defcomponent logout-redirector (simple-window-component)
  ()
  (:documentation "Called to remove cookies and log the user out")
  (:default-initargs
      :stylesheet "css/login.css"))

(defmethod render ((page logout-redirector))
  (let ((session-id (get-cookie "session-id")))
    (with-db
     (delete-records :from [user-sessions]
                     :where [= [session-id] session-id])))
  (set-cookie "user" "")
  (set-cookie "session-id" "")
  (<:meta :http-equiv "refresh"
          :content (format nil "0;url=~a" *url-login*))
    (<:div :id "login"
    (<:div :class "box-title" 
	   (<:h2  "Logging you out..."))
    (<:div :class "box" 
	   (<:img :style "margin: 0 109px;"
		  :width "32px"
		  :height "32px;"
		  :src "/images/ajax-spinner.gif"))))


(defaction do-login-redirect ((page login-redirector))
  (call 'redirect-component :target "main.ucw"))


(defentry-point "externallogin.ucw" (:application *my-app*)
  ((username "") (password ""))
  (if (check-login username password)
      (call 'login-redirector :username username :password password)
      (call 'login-page :msg "Your login information was incorrect. Either we do not have the email address in our database, or the password does not match the email address provided. If you believe you have reached this message in error, please contact us at ")))






#.(clsql:restore-sql-reader-syntax-state)
