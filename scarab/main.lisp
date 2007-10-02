#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#



(in-package :scarab)

#.(clsql:locally-enable-sql-reader-syntax)

(defparameter *report-types* '(software-report))

(defun run-reports ()
  (with-db
    (dolist (user (select 'user :flatp t))
      (unless (disabled user)
        (send-reports user)))))


(defun send-reports (user)
  (dolist (report-type *report-types*)
    (let ((report (make-instance report-type)))
      (do-report report user))))



(defclass report ()
  ())

(defgeneric wants-report-p (report user)
  (:documentation "Returns t if the user wants this report"))

(defgeneric report-body (report user)
  (:documentation "Returns the body of the desired report"))

(defgeneric do-report (report user)
  (:documentation "If the user wants the report, generates and sends it to him"))

(defgeneric report-title (report user)
  (:documentation "The title of the email to send with this report"))

(defmethod do-report ((report report) (user user))
  (when (wants-report-p report user)
      (let ((body (report-body report user)))
        (send-email (list (email user))
		    (report-title report user)
		    body))))

(defclass software-report (report)
  ())

(defmethod wants-report-p ((report software-report) (user user))
  (and (not (disabled user)) (weekly-software-report user)))

(defmethod report-title ((report software-report) (user user))
  (let* ((now (get-time))
         (week-ago (time- now (make-duration :day 7))))
    (format nil "Software report for the week of ~a to ~a" 
            (clsql-sys::print-date week-ago :day)
            (clsql-sys::print-date now :day))))

(defmethod report-body ((report software-report) (user user))
  (with-db
    (let* 
	((agents-p
	  (plusp (caar (with-db (select [count [*]] 
					:from [computers] 
					:where [= [company-id] (company-id user)])))))
	 (installed 
	  (select 
	   'software-event 
	   :flatp t
	   :where [and [> [slot-value 'software-event 'timestamp] 
	   (time- (get-time) (make-duration :day 7))]
	   [= [slot-value 'computer 'id] 
	   [slot-value 'software-event 'computer-id]]
	   [= [slot-value 'computer 'company-id] (company-id user)]
	   [not [= [slot-value 'software-event 'installed] 1]]]))
	 (uninstalled 
	  (select 
	   'software-event 
	   :flatp t
	   :where [and
	   [> [slot-value 'software-event 'timestamp] 
	   (time- (get-time) (make-duration :day 7))]
	   [= [slot-value 'computer 'id] 
	   [slot-value 'software-event 'computer-id]]
	   [= [slot-value 'computer 'company-id] (company-id user)]
	   [= [slot-value 'software-event 'installed] 0]]))
	 (potential-messages (select 'scarab-message))
	 (footer (body (car (nth (random (length potential-messages))
				 potential-messages)))))
      (if agents-p
	  (format nil "
This is your weekly software report. You can turn this report off at <https://archon.paragent.com/preferences.ucw>.

The following software was installed this week:
~{~a~}~%
The following software was uninstalled this week:
~{~a~}~%
----------------------------------------------

Feature of the week:

~a"
		  ;; online computers
		  (if installed
		      (loop for event in installed collect
			   (format nil "'~a' was installed on ~a at ~a.~%"
				   (name event)
				   (name (computer event))
				   (timestamp event)))
		      (list "None"))
		  (if uninstalled
		      (loop for event in uninstalled collect
			   (format nil "'~a' was uninstalled from ~a at ~a.~%"
				   (name event)
				   (name (computer event))
				   (timestamp event)))
		      (list "None"))
		  footer)
	  (format nil "
This is your weekly software report. You can turn this report off at <https://archon.paragent.com/preferences.ucw>. 

It appears that no agents have been installed for this account yet. Once you have added computers to your account, you will receive a weekly report detailing any software that has been installed or uninstalled. The following is a partial sample of the weekly software report:

----------------------------------------------

The following software was installed this week:
'Microsoft Office 2003' was installed on sales-5 at 11:43 AM Tuesday, November 14, 2006.
'Security Update for Windows XP (KB924270)' was installed on IT-2 at 3:09 AM Wednesday, November 15, 2006.
'Security Update for Windows XP (KB923980)' was installed on database-3 at 3:10 AM Wednesday, November 15, 2006.
'Security Update for Windows XP (KB924270)' was installed on testing-1 at 3:08 AM Sunday, November 19, 2006.

The following software was uninstalled this week:
'Security Update for Windows XP (KB913433)' was uninstalled from IT-2 at 3:09 AM Wednesday, November 15, 2006.
'Security Update for Windows XP (KB913433)' was uninstalled from database-3 at 3:10 AM Wednesday, November 15, 2006.
'Security Update for Windows XP (KB913433)' was uninstalled from testing-1 at 6:09 AM Wednesday, November 15, 2006.
'Norton Antivirus' was uninstalled on accounting-4 at 2:21 PM Thursday, November 16, 2006.

----------------------------------------------

Feature of the week:

~a" footer)))))
  
#.(clsql:restore-sql-reader-syntax-state)
