#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#



(in-package :reaver)

#.(clsql:locally-enable-sql-reader-syntax)

(defparameter *disabled-accounts* 0)
(defparameter *new-accounts* 0)
(defparameter *total-trials* 0)
(defparameter *total-agents* 0)
(defparameter *accounts-with-agents* 0)

(defun age (company)
  (handler-case
      (let ((age 
	     (car 
	      (with-db 
		  (query 
		   (format nil 
			   "select TO_DAYS(NOW()) - TO_DAYS(CREATED) from COMPANIES where id=~a" 
			   (id company))
		   :flatp t)))))
	age)
    (t () nil)))

(defun computer-count (company)
  (handler-case
      (let ((count (car (with-db (select [count [id]] 
					 :from [computers]
					 :flatp t
					 :where [= [company-id] (id company)])))))
	count)
    (t () nil)))

(defun disable-account (company)
  (setf (disabled company) t)
  (db:with-db (clsql:update-records-from-instance company)))

(defun process-trial-account (company)
  (let ((age (age company))
	(count (computer-count company)))
    (when (plusp count)
      (incf *accounts-with-agents*)
      (incf *total-agents* count))
    (incf *total-trials*)
    (cond
      ((= age 1)
       (incf *new-accounts*))
      ((and (= age 5) (zerop count))
       (send-report (make-instance 'trial-email-no-agents 
				   :company company 
				   :discount-code "AxDbcj") company))
      ((and (= age 5) (plusp count))
       (send-report (make-instance 'trial-email-with-agents 
				   :company company
				   :discount-code "AxDbcj") company))
      ((= age 10)
       (send-report (make-instance 'trial-email-inventory :company company) company))
      ((= age 15)
       (send-report (make-instance 'trial-email-search :company company) company))
      ((= age 20)
       (send-report (make-instance 'trial-email-remote :company company) company))
      ((= age 28)
       (send-report (make-instance 'trial-email-final :company company) company))
      ((> age 31)
       (send-report (make-instance 'trial-email-close :company company) company)
       (disable-account company)
       (incf *disabled-accounts*)))))

(defgeneric send-report (report company)
  (:method ((report trial-report) (company company))
  (let ((users (with-db (select 'user 
				  :from [users] 
				  :flatp t 
				  :where [= [company-id] (id company)])))
	(plain-body (report-plain-body report))
	(html-body (report-html-body report))
	(title (report-title report)))
    (when users
      (dolist (user users)
	(send-email (email user) title plain-body :html-message html-body))))))

(defparameter *disabled-accounts* 0)
(defparameter *new-accounts* 0)
(defparameter *total-trials* 0)
(defparameter *total-agents* 0)
(defparameter *accounts-with-agents* 0)

(defun send-admin-report ()
  (let ((avg-count (if (plusp *accounts-with-agents*)
		       (/ *total-agents* *accounts-with-agents*)
		       0)))
    (send-email (list "tritchey@paragent.com" "jcheesman@paragent.com")
		(format nil 
			"+~a | -~a | ~a | Paragent Daily Trial Account Report"
			*new-accounts* *disabled-accounts* *total-trials*)
		(format nil 
			"Total Trials: ~a~%New Trials: ~a~%Disabled Accounts: ~a~%Trials With Agents: ~a~%Average Agent Count: ~a~%"
			*total-trials* *new-accounts* *disabled-accounts* *accounts-with-agents* avg-count))))


(defun process-trial-accounts ()
  (with-db
    (dolist (company (select 'company 
			  :from [companies]
			  :flatp t
			  :where [= [slot-value 'company 'level] 0]))
      (when (not (disabled company))
        (process-trial-account company))))
  (send-admin-report))
  
#.(clsql:restore-sql-reader-syntax-state)
