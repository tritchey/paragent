#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#


;;; functions for monitoring the running templar instances we know about

(in-package :archon)

#.(clsql:locally-enable-sql-reader-syntax)

(defconstant +unix-to-lisp-time-offset+ (encode-universal-time 0 0 0 1 1 1970 0))

(defmethod remove-all-alerts ((client client))
  (send-message client '(remove-all-alerts)))

(defmethod add-alert ((client client) (alert alert))
  (send-message client `(add-alert ,(id alert) ,(type-id alert) ,(args alert))))

(defmethod send-alerts ((client client))
  (remove-all-alerts client)
  (db:with-db
      (dolist (alert (alerts (computer client)))
	(add-alert client alert))))

(defun update-alerts (&rest args)
  (if (= (length args) 1)
      (let ((ids (car args)))
	(if (atom ids)
	    (let ((client (gethash ids *clients-by-computer-id*)))
	      (when client
		(send-alerts client)))
	    (dolist (id ids)
	      (let ((client (gethash id *clients-by-computer-id*)))
		(when client
		  (send-alerts client)))))
	t)
      nil))

(defclass alert-event ()
  ((summary :initarg :summary
	    :accessor summary
	    :initform nil)
   (decription :initarg :description
	       :accessor description
	       :initform nil)
   (note :initarg :note
	 :accessor note
	 :initform nil)
   (severity :initarg :severity
	     :accessor severity
	     :initform nil)
   (timestamp :initarg :timestamp
	      :accessor timestamp
	      :initform nil)
   (email-to :initarg :email-to
	     :accessor email-to
	     :initform nil)))

(defgeneric create-alert-event (alert data computer))

(defmethod create-alert-event ((alert alert) data computer)
  (create-alert-event (impl alert) data computer))

(defmethod create-alert-event ((alert db::alert-impl) data computer)
  (let ((timestamp (clsql:get-time)))
    (make-instance 'alert-event
                   :summary "Alert Error"
                   :description "An invalid alert type was created"
		   :note "Please contact support@paragent.com about this incident"
                   :severity 0
                   :timestamp timestamp
                   :email-to (email-to (db-obj alert)))))

(defun get-alert-prop (item data)
  (second (assoc item data :test #'equal)))

(defmacro alert-event-creator (alert-type summary description props)
  `(defmethod create-alert-event ((alert ,alert-type) data computer)
     (let ((data (caar data))
	   (timestamp (clsql:utime->time (+ (cadr data) +unix-to-lisp-time-offset+))))
       ,(when (and (equal (length props) 1)
		   (eql (car props) :name))
	      `(declare (ignore data)))
       (make-instance 'alert-event
                      :severity (severity (db-obj alert))
                      :email-to (email-to (db-obj alert))
		      :note (note (db-obj alert))
                      :timestamp timestamp
                      :summary ,summary
                      :description (format nil ,description
                                           ,@(mapcar
                                               (lambda (prop)
                                                 (cond
                                                   ((eql :name prop)
                                                    `(name computer))
                                                   ((stringp prop)
                                                     `(get-alert-prop ,prop data))
                                                   (t
                                                     prop)))
                                               props))))))

(defmethod create-alert-event ((alert alert-memory) data computer)
  (let ((timestamp (clsql:utime->time (+ (cadr data) +unix-to-lisp-time-offset+))))
    (make-instance 'alert-event
		   :severity (severity (db-obj alert))
		   :email-to (email-to (db-obj alert))
		   :note (note (db-obj alert))
		   :timestamp timestamp
		   :summary "Low Memory"
		   :description (format nil "Low memory alert for ~a"
					(name computer)))))

(defmethod create-alert-event ((alert alert-software) data computer)
  (let ((timestamp (clsql:utime->time (+ +unix-to-lisp-time-offset+ (cadr data))))
	(description 
	 (format nil 
		 "The following software was installed on ~a: ~{~A~#[~:;, ~]~}"
		 (name computer)
		 (mapcar
		  (lambda (software)
		    (format nil "~a" (first software)))
		  (car data)))))
    (make-instance 'alert-event
		   :severity (severity (db-obj alert))
		   :email-to (email-to (db-obj alert))
		   :note (note (db-obj alert))
		   :timestamp timestamp
		   :summary "Software installed"
		   :description description)))

(defmethod create-alert-event ((alert alert-user-logon) data computer)
  (let ((timestamp (clsql:utime->time (+ +unix-to-lisp-time-offset+ (cadr data))))
        (description
          (format nil
                  "The user '~{~A~#[~:;, ~]~}' logged on to ~a."
                  (car data)
                  (name computer))))
    (make-instance 'alert-event
                   :severity (severity (db-obj alert))
                   :email-to (email-to (db-obj alert))
                   :note (note (db-obj alert))
                   :timestamp timestamp
                   :summary "User logged on"
                   :description description)))

(defmethod create-alert-event ((alert alert-user-logoff) data computer)
  (let ((timestamp (clsql:utime->time (+ +unix-to-lisp-time-offset+ (cadr data))))
        (description
          (format nil
                  "The user '~{~A~#[~:;, ~]~}' logged off of ~a."
                  (car data)
                  (name computer))))
    (make-instance 'alert-event
                   :severity (severity (db-obj alert))
                   :email-to (email-to (db-obj alert))
                   :note (note (db-obj alert))
                   :timestamp timestamp
                   :summary "User logged off"
                   :description description)))


(defmethod create-alert-event ((alert alert-computer-offline) data computer)
  (let ((timestamp (clsql:get-time)))
    (make-instance 'alert-event
                   :summary "Computer offline"
                   :description (format nil "~a went offline." 
					(name computer))
		   :note (note (db-obj alert))
                   :severity 0
                   :timestamp timestamp
                   :email-to (email-to (db-obj alert)))))

(alert-event-creator alert-cd "CD inserted"
                     "A CD named '~a' was inserted into drive ~a on ~a." 
		     ("VolumeName" "Drive" :name))

(alert-event-creator alert-process-stop "Process stopped"
                     "The process '~a' stopped on ~a." 
		     ("Description" :name))

(alert-event-creator alert-service-stop "Service stopped"
                     "The service '~a' stopped on ~a with exit code ~a." 
		     ("Caption" :name "ExitCode"))

(alert-event-creator alert-service-start "Service started"
                     "The service '~a' started on ~a." ("Caption" :name))

(alert-event-creator alert-processor "Processor usage high"
                     "The processor level rose to ~a on ~a." ("cpu" :name))

(alert-event-creator alert-hard-drive "Hard drive filling up"
                     "The hard drive is getting full on ~a." (:name))

(alert-event-creator alert-pnp-added "Plug and Play device added"
		     "The plug and play device '~a' was inserted in ~a." ("Caption" :name))

(alert-event-creator alert-pnp-removed "Plug and Play device removed"
		     "The plug and play device '~a' was removed from ~a." ("Caption" :name))


(defgeneric format-alert-message (computer alert))

(defgeneric alert-offline (computer))

(defmethod format-alert-message ((computer computer) (alert alert-event))
  (format nil "Computer: ~a~%Timestamp: ~a~%Severity: ~a~%Summary: ~a~%Description: ~a~%Note:~%~a~%"
	  (name computer)
	  (multiple-value-bind (usec second minute hour day month year dow)
	      (clsql:decode-time (timestamp alert))
	    (declare (ignore usec second minute hour))
	    (format nil "~a, ~a ~d, ~d" 
		    (clsql-sys::day-name dow) 
		    (clsql:month-name month) day year))
	  (case (severity alert)
	    (0 "Information")
	    (6 "Low")
	    (8 "Medium")
	    (10 "High")
	    (otherwise "Unknown"))
	  (summary alert)
	  (description alert)
	  (note alert)))

(defmethod alert-offline ((computer computer))
  (when computer
    (db:with-db
	(let ((recorded nil)
	      (event-id nil))
	  (dolist (alert (db:type-alerts computer +alert-computer-offline+))
	    (unless recorded
	      (setf recorded t)
	      (let ((name (name computer)))
		(setf event-id (record-event computer
					     :summary (format nil "~A offline" name)
					     :description (format nil "The computer ~A went offline" name)
					     :severity :low))))
	    (let ((aevent (create-alert-event alert nil computer)))
	      (clsql:update-records-from-instance
	       (make-instance 'alert-event-link 
			      :event-id event-id
			      :alert-id (id alert)))
	      (send-email (email-to aevent)
			  (description aevent)
			  (format-alert-message computer aevent))))))))

#.(clsql:restore-sql-reader-syntax-state)


    
		 
