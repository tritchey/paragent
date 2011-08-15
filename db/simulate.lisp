#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

;;; Contains nearly all of the database/object mappings


(in-package :db)

#.(clsql:locally-enable-sql-reader-syntax)

(defparameter *random-event-text* '(("Software Installed" "The software Microsoft Office 2003 was installed on ~a")
				    ("Service added" "The service Norton Antivirus  was installed on ~a")
				    ("Hotfix installed" "The hotfix KB936929 was installed on ~a")
				    ("Low Space on Hard Drive" "Hard Drive C: is below 10% on ~a")
				    ("Plug and Play device added" "The Plug and Play device USB Flash Drive was added on ~a")
				    ("Plug and Play device removed" "The Plug and Play device USB Flash Drive was removed from ~a")
				    ("Processor usage high" "Processor usage above 90% on ~a")
				    ("Computer offline" "The computer ~a went offline")))

(defun generate-random-events-for-day (days-in-past num)
  (let ((computers (mapcar #'car (with-db (select 'computer :where [= [company-id] 1])))))
    (dolist (computer computers)
      (generate-random-events-for-computer days-in-past num computer))))

(defun generate-random-events-for-computer (days-in-past num computer)
  (dotimes (num (random num))
    (let* ((id (id computer))
	   (item (random 8))
	   (summary (car (nth item *random-event-text*)))
	   (description (format nil (second (nth item *random-event-text*)) (name computer)))
	   (company-id (company-id computer))
	   (timestamp (clsql:time- (clsql:get-time) (clsql:make-duration :day days-in-past)))
	   (severity-id 0)
	   (event (make-instance 'event :summary summary
				 :description description
				 :severity-id severity-id
				 :timestamp timestamp
				 :computer-id id
				 :company-id company-id)))
      (with-db (insert-and-update event)))))

(defun generate-random-events-for-client (client days-in-past num)
  (let ((computers (mapcar #'car (with-db (select 'computer :where [= [client-id] client])))))
    (dolist (computer computers)
      (generate-random-events-for-computer days-in-past num computer))))


(defun generate-random-events ()
  (dotimes (i 7)
    (generate-random-events-for-day i  (+ 5 (random 15)))))

#.(clsql:restore-sql-reader-syntax-state)
