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

(defvar *clients-by-guid* (make-hash-table :test #'eql)
  "hash table of clients based on their guid as assigned by arbiter")

(defvar *clients-by-name* (make-hash-table :test #'equal)
  "convenience hash table for looking up clients by their name")

(defvar *client-message-broker* nil)

(defvar *current-client-version* nil
  "the latest client that should be running on everyone")

(defvar *clients-by-computer-id* (make-hash-table))

(defvar *updating-clients* (make-hash-table))

(defvar *templar-command-whitelist* '(heartbeat 
				      event-triggered
				      connected
				      disconnected))

(defvar *templar-connection-string* "127.0.0.1"
  "this is the string we put in the database for a client to let nexus know which archon instance to send a message")


(defclass client ()
  ((guid :accessor guid
	 :initarg :guid
	 :initform nil)
   (arbiter-connection :accessor arbiter-connection
		       :initarg :arbiter-connection
		       :initform nil)
   (name :accessor name
         :initform nil)
   (company :accessor company
	    :initform nil)
   (company-id :accessor company-id
	       :initform nil)
   (computer-id :accessor computer-id
		:initform nil)
   (computer :accessor computer
	     :initform nil)
   (messages :accessor messages
	     :initform (make-hash-table :test #'eql))
   (old-computer :accessor old-computer-p
		   :initform nil)
   (heartbeat :accessor heartbeatp
	      :initform t)
   (validp :accessor validp
	  :initform nil))
  (:documentation "a remote computer connecting to us"))

(defgeneric client-online (client)
  (:documentation "command to call when the client is fully online and the handshake has been completed. It collects all of the client properties, and configures alerts"))

(defgeneric heartbeat (client)
  (:documentation "handle the heartbeat message from a client"))

(defgeneric disconnected (client)
  (:documentation "the client has been disconnected from arbiter"))

(defgeneric disconnect-client (client &key record-offline-p))

(defgeneric duplicate-client (client))

(defgeneric client-version (client &rest args)
  (:documentation "check the client version, and send an update command if necessary"))

(defgeneric client-identification (client &rest args)
  (:documentation "process the identification message from the client"))

(defgeneric negotiate-connection (client &key)
  (:documentation "kick off the client handshake"))

(defgeneric recv (client message)
  (:documentation "receive a message for a client"))

(defgeneric send (client command)
  (:documentation "send a commend to a client"))

(defgeneric remove-all-alerts (client))

(defgeneric add-alert (client alert))

(defgeneric send-alerts (client))

(defgeneric make-unique-msgid (client))

(defgeneric send-message (client command &optional fn))

(defgeneric get-computer (client))

(defun check-company-secret (company secret)
  (db:with-db
    (clsql:select 'company 
		  :where [and [= [name] company] [= [secret] secret]] 
		  :refresh t :limit 1)))

(defmethod initialize-instance :after ((client client) &rest args)
  (declare (ignore args))
  (setf (gethash (guid client) *clients-by-guid*) client)
  (negotiate-connection client))

(defmethod client-online ((client client))
  (let ((computer (computer client)))
      (setf (last-online computer) (clsql:get-time))
      (db:with-db (clsql:update-records-from-instance computer)))
  (query-all-props client))

(defmethod heartbeat ((client client))
  (setf (heartbeatp client) t))

(defmethod disconnected ((client client))
  (record "~A.~A disconnected" (name client) (company client))
  (disconnect-client client))

(defmethod disconnect-client ((client client) &key (record-offline-p t))
  (let* ((guid (guid client))
	 (name-hash (list (name client) (company client)))
	 (computer-id (computer-id client))
	 (client-guid-by-name (gethash name-hash *clients-by-name*))
	 (client-by-computer-id (gethash computer-id *clients-by-computer-id*)))
    (setf (validp client) nil)
    ;; check to see if the entries in the name and computer-id
    ;; hashes match the one that disconnected
    (when (equal client-guid-by-name guid)
      (remhash name-hash *clients-by-name*))
    (when (and client-by-computer-id (equal (guid client-by-computer-id)
					    guid))
      (remhash computer-id *clients-by-computer-id*)
      (when record-offline-p
	(record-offline-event (computer client))))

    ;; atomically remove our guid from the computer object
    ;; in the db - we need to do this to make sure another
    ;; archon doesn't already have ownership of this client
    (db:with-db
	(db:with-restarting-transaction
	  (let ((computer (computer client)))
	    (when computer
	      (clsql:update-instance-from-records computer)
	      (when (= (archon-connection computer) (guid (arbiter-connection client)))
		(setf (archon-connection computer) 0)
		(clsql:update-records-from-instance computer))))))
      
    ;; always remove from the guid hash
    (remhash guid *clients-by-guid*)))

(defmethod duplicate-client ((client client))
  (send-message client '(duplicate-client))
  (send-message (arbiter-connection client) `(disconnect-client ,(guid client))))
  

(defmethod client-version ((client client) &rest args)
  (let ((update (update-for-version 
		 args 
		 (db:with-db (clsql:select 'db:templar-update :refresh t))))
	(key (list (name client) (company client))))
    (if update
	(if (gethash key *updating-clients*)
	    (record "WARNING: ~a failed to update" key)
	    (progn
	      (setf (gethash key *updating-clients*) t)
	      (send-message client (format nil "(update ~S)" update) 
			    #'(lambda (client &rest args)
				(declare (ignore client))
				(record "templar update: ~A~%" args)))))
	(progn
	  (setf (gethash key *updating-clients*) nil)
	  (client-online client)))))

(defmethod client-identification ((client client) &rest args)
  (if (= (length args) 3)
      (let ((name (car args))
	    (company (cadr args))
	    (secret (third args)))
	(if (and name
		 company
		 (check-company-secret company secret))
	    ;; check for duplicate client entry
	    (if (gethash (list name company) *clients-by-name*)
		(duplicate-client client)
		(progn
		  (setf (name client) name) 
		  (setf (company client) company) 
		  (setf (company-id client) (company-id-for-name company))
		  ;; create a computer object so we can get the computer-id
		  ;; and set that value in our client object
		  ;; also, we need to set the connection string
		  ;; and the online status
		  (let ((computer (get-computer client)))
		    (setf (computer client) computer)
		    ;(setf (old-computer-p client) in-db)
		    (setf (computer-id client) (id computer))
		    (setf (gethash (id computer) *clients-by-computer-id*) client)
		    (setf (archon-connection computer) (guid (arbiter-connection client)))
		    (db:with-db (clsql:update-records-from-instance computer))
		    (record-online-event computer))
		  ;; add the client to our duplicate-client-detector hash
		  (record "~A.~A connected" name company)
		  (setf (gethash (list name company) *clients-by-name*) (guid client))
		  (setf (validp client) t)
		  (send-message client '(version) #'client-version)))
	    (progn
	      (record "unable to find matching company/secret")
	      (disconnect-client client))))
      (progn
	(record "incorrect arg count returned from client")
	(disconnect-client client))))


(defmethod negotiate-connection ((client client) &key)
  (send-message client '(identification) #'client-identification))

(defmethod recv ((client client) message)
  (handler-case
      (let* ((msgid (car message))
	     (callback (gethash msgid (messages client)))
	     (body (cadr message))
	     (result (if (atom body) (list body) body)))
	(unwind-protect
	     (if callback
		 (apply callback client result)
		 (when (member (car result) *templar-command-whitelist*)
		   (apply (car result) client (cdr result))))
	  (remhash msgid (messages client))))
    (undefined-function ()
      (record "undefined-function returned from ~A \"~A\"" 
	      (name client) message))))

(defmethod send ((client client) command)
  (write-message (arbiter-connection client) command))

(defmethod make-unique-msgid ((client client))
  :documentation "create a unique message identifier by 
                  taking the 15 low-order bits of the 
                  current universal time, and shoving 
                  them on top of a random 16 bit number
                  (we keep the top bit clear, otherwise 
                  we get sign-wrappage"
  (let ((id (dpb (get-universal-time) (byte 15 16) (random #xFFFF))))
    (if (gethash id (messages client))
	(make-unique-msgid client)
	id)))

(defun create-message (guid msgid command)
  (let ((*package* (find-package :archon))
	(*print-pretty* nil))
    (if (stringp command)
	(format nil "(~S (~S ~A))~%" guid msgid command)
	(format nil "~S~%" (list guid (list msgid command))))))

(defmethod send-message ((client client) command &optional fn)
    (let ((msgid (make-unique-msgid client)))
      (when fn
	(setf (gethash msgid (messages client)) fn))
      (send client (create-message (guid client) msgid command))))

(defun company-id-for-name (company)
  (let* ((company 
	  (car (db:with-db (clsql:select 'company 
	                                 :where [= [name] company] 
	                                 :refresh t :flatp t :limit 1)))))
    (if company	(id company) nil)))

(defun client-for-name (name)
  (let ((computer (caar (db:with-db (clsql:select 'computer
						  :where [= [name] name])))))
    (gethash (id computer) *clients-by-computer-id*)))

(defmethod get-computer ((client client))
  :documentation "get the database computer object for the client 
                  connection, creating instance if necessary"
  (let* ((name (name client))
	 (company-id (company-id client))
	 (computer 
	  (car 
	   (db:with-db (clsql:select 'computer 
				       :where [and [= [name] name] [= [company-id] company-id]]
				       :refresh t :flatp t :limit 1)))))
    (if computer
        (let ((updated-p (not (gethash (list (name client) (company client)) *updating-clients*))))
          (setf (old-computer-p client) updated-p)
          (values computer updated-p))
        (progn
          (setf (old-computer-p client) nil)
	  (db:with-db
	    (clsql:update-records-from-instance
	     (make-instance 'computer 
			    :name name 
			    :company-id company-id 
			    :connection *templar-connection-string*
	                    :online nil))
	    (values (car (clsql:select 'computer 
				       :where [and [= [name] name] [= [company-id] company-id]]
				       :refresh t :flatp t :limit 1))
		    nil))))))

(defun update-client (name)
  (send-message 
   (client-for-name name) 
   '(update (() 
	     ("Templar.exe" 
	      "http://archon.paragent.com/updates/0.3.2772/Templar.exe") 
	     ("version.dat" 
	      "http://archon.paragent.com/updates/0.3.2772/version.dat")))))

#.(clsql:restore-sql-reader-syntax-state)
