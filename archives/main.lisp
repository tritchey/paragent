#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

(in-package :archives)

#.(clsql:locally-enable-sql-reader-syntax)


(defun loop-endlessly ()
  (do () (nil)
    (run)
    (sleep 30)))

(defun run ()
  (with-db
    (dolist (company (select 'company :flatp t))
      (format t "Company: ~a~%" (name company))
      (dolist (email-account (select 'ticket-email :flatp t :where [= [company-id] (id company)]))
        (format t " Account: ~a~%" (username email-account))
	(let ((server (make-server email-account)))
	  (handler-case
	      (mel:map-messages
	       (lambda (m)
		 (process-email m company))
	       server)
	    (SB-BSD-SOCKETS:OPERATION-TIMEOUT-ERROR (e)
	      (format t "RUN: ETIMEDOUT on account ~a~%" e))
	    (t (e)
	      (format t "~a~%" e)))
	  (mel:close-folder server))))))

(defun make-server (ticket-email)
  (declare (ticket-email ticket-email))
  (make-instance 'mel:pop3-folder :host (host ticket-email) :port (port ticket-email)
                 :username (username ticket-email) :password (password ticket-email)))


(defun process-email (message company)
  (let ((subject (mel.mime::subject message))
        (from (mel:address-spec (mel:from message)))
        (body (body message)))
    (let ((case-num (extract-case-number subject)))
      (if case-num
          (process-reply case-num message company)
          (let* ((ticket (create-ticket message company))
                 (ticket-id (ticket-id ticket)))
            (send-ticketing-email 
             company
             (list from)
             (format nil "[Case ~a] Your ticket has been submitted: ~a" ticket-id subject)
             (format nil "Your ticket has been added to the system, and will be reviewed by a technician shortly. Thank you for your submission.~%~%------- [ticket message] -------~%~a" body)))))
    (mel:delete-message message)
    ))


(defun process-reply (case-num message company)
  (let ((ticket (car (select 'ticket :flatp t :limit 1
                             :where [and
                             [= [slot-value 'ticket 'company-id] (id company)]
                             [= [slot-value 'ticket 'ticket-id] case-num]]))))
    (when ticket
      (let ((response (make-instance 'ticket-response :ticket-id (id ticket)
                                     :sender (mel:address-spec (mel:from message))
                                     :body (body message))))
        (update-records-from-instance response)))))

(defun extract-case-number (subject)
  (cl-ppcre:register-groups-bind 
    (number) (".*\\[Case ([1-9][0-9].*)\\].*" subject)
    number))

(defun create-ticket (message company)
  (with-db
    (let ((subject (mel.mime::subject message))
          (from (mel:address-spec (mel:from message)))
          (body (body message))
          (ticket nil))
      (with-restarting-transaction
        (setf ticket (make-instance 'ticket :company-id (id company)
                                    :state db::+ticket-status-open+ :priority db::+ticket-priority-medium+
                                    :assigned-user-id nil
                                    :subject subject
                                    :body body
                                    :response-email from
                                    :ticket-id (get-next-ticket-id (id company))))
        (clsql:update-records-from-instance ticket))
      ticket)))

(defmethod body (message)
  (with-output-to-string (str)
                         (with-open-stream (stream (mel:message-body-stream message))
                                           (loop for c = (read-char stream nil nil)
                                                 while c do (write-char c str)))))   
   
   
#.(clsql:restore-sql-reader-syntax-state)
