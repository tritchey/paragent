#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#


(in-package :archon)

#.(clsql:locally-enable-sql-reader-syntax)

(defgeneric record-event (computer &key summary description severity timestamp))

(defgeneric record-online-event (computer))

(defgeneric record-offline-event (computer))

(defgeneric check-hard-drive (alert client computer data))

(defgeneric event-triggered (client &rest args))

(defmethod record-event ((computer computer) &key summary description severity timestamp)
  (when computer
    (let ((id (id computer))
	  (company-id (company-id computer))
	  (timestamp (or timestamp (clsql:get-time)))
	  (severity-id (case severity
			 (:note 0)
			 (:low 6)
			 (:medium 8)
			 (:high 10)
			 (t severity))))
      (let ((event (make-instance 'event :summary summary
                                  :description description
                                  :severity-id severity-id
                                  :timestamp timestamp
                                  :computer-id id
				  :company-id company-id)))
        (db:with-db
	    (insert-and-update event))
        (id event)))))

(defmethod record-online-event ((computer computer))
  (unless (online computer)
    (setf (online computer) t)
    (db:with-db (clsql:update-records-from-instance computer))))

(defmethod record-offline-event ((computer computer))
  (when (online computer)
    (setf (online computer) nil)
    (db:with-db (clsql:update-records-from-instance computer))
    (alert-offline computer)))


(defmethod check-hard-drive ((alert alert) (client client) (computer computer) data)
  (query-let ((drives (wmi-query client "Win32_LogicalDisk WHERE DriveType=3" "Caption"))
	      (sizes (wmi-query client "Win32_LogicalDisk WHERE DriveType=3" "Size"))
	      (frees (wmi-query client "Win32_LogicalDisk WHERE DriveType=3" "FreeSpace")))
      client
    (let* ((threshold (car (args alert)))
	   (timestamp (clsql:time+ (clsql:utime->time (cadr data))
				   (clsql:make-duration :year 70))))
      (handler-case 
	  (mapcar 
	 #'(lambda (drive size free)
	     (when (< free (* size (/ threshold 100))) 
	       (db:with-db
		   (let* ((aevent (make-instance 'alert-event
						 :summary "Low Space on Hard Drive"
						 :description (format nil 
								      "~A drive ~A is below ~A" 
								      (name computer) drive threshold)
						 :severity (severity alert)
						 :email-to (email-to alert)
						 :note (note alert)
						 :timestamp timestamp))
			  (event-id (record-event computer
						  :summary (summary aevent)
						  :description (description aevent)
						  :severity (severity aevent)
						  :timestamp (timestamp aevent))))
		     (clsql:update-records-from-instance
		      (make-instance 'alert-event-link 
				     :event-id event-id
				     :alert-id (id alert)))
		     (send-email (email-to aevent)
				 (description aevent)
				 (format-alert-message computer aevent))))))
	 (car drives) (car sizes) (car frees))
      (t (e)
	(record "~A~%" e))))))

(defmethod event-triggered ((client client) &rest args)
  (when (validp client)

    (let* ((id (car args))
	   (data (cdr args))
	   (alert (caar (db:with-db (clsql:select 'alert :where [= [id] id]))))
	   (computer (computer client))
	   (alert-link (db:with-db (clsql:select 'alert-computer-link 
						 :where [and [= [alert-id] id]
						 [= [computer-id] (id computer)]]))))
      (handler-case 
	  (when (and alert alert-link)
	    (if (equal (type-id alert) db:+alert-hard-drive+)
		(check-hard-drive alert client computer data)
		(db:with-db
		    (let* ((aevent (create-alert-event alert data computer))
			   (desc (description aevent)))
		      (unless (or (not desc)
				  (equal desc "")
				  (search "Microsoft DirectMusic" desc)
				  (search "Microsoft Streaming" desc)
				  (search "BDA Slip De-Framer" desc)
				  (search "Codec" desc)
				  (search "Microsoft Kernel" desc)
				  (search "RAS Async Adapter" desc)
				  (search "Generic volume" desc)
				  (search "Microsoft WINMM WDM Audio Compatibility Driver" desc))
			(let ((event-id (record-event computer
						      :summary (summary aevent)
						      :description (description aevent)
						      :severity (severity aevent)
						      :timestamp (timestamp aevent))))
			  (clsql:update-records-from-instance
			   (make-instance 'alert-event-link 
					  :event-id event-id
					  :alert-id (id alert)))
			  (send-email (email-to aevent)
				      (description aevent)
				      (format-alert-message computer aevent))))))))
	(t (e)
	  (record "~A: ~A ~A ~A ~A ~A~%" e id data alert computer alert-link))))))
  
#.(clsql:restore-sql-reader-syntax-state)
