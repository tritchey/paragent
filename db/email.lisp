#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#


(in-package :db)


(defmethod send-email ((to string) subject message &key html-message)
  (when *email-server*
    (handler-case 
	(let ((to-list (split-sequence:split-sequence #\, to :remove-empty-subseqs t)))
	  (when to-list
	    (cl-smtp:send-email *email-server*
				*email-from-address*
				to-list
				subject
				message
				:html-message html-message
				:port *email-smtp-port*
				:authentication (if *email-smtp-authentication*
						    (list :login *email-account-name* *email-account-password*)
						    nil)
				:buffer-size 1
				:ssl *email-smtp-ssl*)
	    t))
      (t (e)
	(record "SEND-EMAIL: error on sending email~%TO: ~a~%SUBJECT: ~a~%MESSAGE:~%~a~%ERROR: ~a" to subject message e)
	nil))))

(defmethod send-email ((to list) subject message &key html-message)
  (when *email-server*
    (handler-case 
	(let ((to-list to))
	  (when to-list
	    (cl-smtp:send-email *email-server*
				*email-from-address*
				to-list
				subject
				message
				:html-message html-message
				:port *email-smtp-port*
				:authentication (if *email-smtp-authentication*
						    (list :login *email-account-name* *email-account-password*)
						    nil)
				:buffer-size 1
				:ssl *email-smtp-ssl*)
	    t))
      (t (e)
	(record "SEND-EMAIL: error on sending email~%TO: ~a~%SUBJECT: ~a~%MESSAGE:~%~a~%ERROR: ~a" to subject message e)
	nil))))

(defmethod send-ticketing-email ((user user) to subject body)
  "If the company has an email account set up, send email from there. Otherwise, use the default email."
  (declare (list to))
  (declare (string subject body))
  (with-db
   (let ((email-account (car (ticket-emails (company-id user)))))
     (if email-account
         (handler-case
          (progn
            (cl-smtp:send-email (host email-account)
                                (username email-account)
                                to
                                subject
                                body
                                :authentication (list :login (username email-account) (password email-account))
                                :buffer-size 1)
            t)
          (t ()
            (update-records-from-instance
             (make-instance 'logged-error :company-id (company-id user)
					  :body (format nil "There was an error while attempting to send an email from the ticketing system using the configured email account (user '~a' on the server '~a'). Please check your ticket email account on the setup page. If you are still having difficulty, please contact support@paragent.com.~% TO: ~a~% SUBJECT: ~a~% BODY:~%~a~%" (username email-account)
							(host email-account)
							to subject body)))
            nil))
	 (send-email to subject body)))))



(defmethod send-ticketing-email ((company company) to subject body)
  (declare (list to))
  (declare (string subject body))
  (with-db
   (let ((email-account (car (ticket-emails (id company)))))
     (if email-account
         (handler-case
          (progn
            (cl-smtp:send-email (host email-account)
                                (username email-account)
                                to
                                subject
                                body
                                :authentication (list :login (username email-account) (password email-account))
                                :buffer-size 1)
            t)
          (t ()
            (update-records-from-instance
             (make-instance 'logged-error :company-id (id company)
                            :body (format nil "Unable to send email from '~a'" (host email-account))))
            nil))
         (send-email to subject body)))))

