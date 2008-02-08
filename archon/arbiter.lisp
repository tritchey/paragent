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

(defvar *arbiter-connection* nil
  "connection with which we are connected to arbiter")

(defvar *arbiter-timeout* 30)

(defvar *arbiter-command-whitelist* '(assign-guid))

(defvar *enable-email-alerts* nil)

(defclass arbiter-connection (client-connection)
    ((guid :accessor guid
	   :initform nil))
  (:documentation "an arbiter connection object"))

(defgeneric assign-guid (connection id))

(defmethod assign-guid ((connection arbiter-connection) id)
  (record "We have been assigned GUID: ~A" id)
  (setf (guid connection) id))

(defmethod write-message :around ((connection arbiter-connection) message)
  (handler-case
      (call-next-method)
    (unix-error:ebadf ()
      (record "EBADF when sending message to ARBITER"))))

(defmethod read-message ((connection arbiter-connection) message)
  (handler-case
      (let* ((*read-eval* nil)
	     (msg (read-from-string message :external-format :utf-8))
	     (guid (car msg))
	     (body (cadr msg))
	     (result (if (atom body) (list body) body)))
	(cond
	  ((zerop guid)
	   ;; a message from arbiter
	   (when (member (car result) *arbiter-command-whitelist*)
	     (apply (car result) connection (cdr result))))
	    ;; message is for one of the client instances
	  ((= guid 1)
	   ;; a message from nexus
	   (when (member (car result) *nexus-command-whitelist*)
	     (apply (car result) (cdr result))))
	  ((plusp guid)
	   (let ((client (gethash guid *clients-by-guid*)))
	     (if client
		 ;; we know about this client
		 (recv client body)
		 ;; we have no idea who this guy is
		 (make-instance 'client :guid guid :arbiter-connection connection))))))
    (end-of-file ()
      (record "end-of-file on message: ~A" message))
    (t (e)
      (record "arbiter.lisp: (read-message) had an unhandled case ~a~%~a" e message) 
      (when *enable-email-alerts*
	(send-email *administrator-notification-email*
		    "ARCHON: arbiter.lisp (read-message) caught an unhandled exception"
		    (format nil "ARCHON: ~A" e))))))

(defmethod disconnect-event :after ((connection arbiter-connection))
  (record "DISCONNECT-EVENT: disconnected from arbiter - ")
  (maphash #'(lambda (k v) (declare (ignore k)) 
		     (disconnect-client v :record-offline-p nil)) *clients-by-guid*)
  (setf *arbiter-connection* nil))
	
(defmethod send-message ((client arbiter-connection) command &optional fn)
  (declare (ignore fn))
  (let ((*print-pretty* nil))
    (labels ((format-message (command)
	       (if (stringp command)
		   (format nil "(0 (~A))~%" command)
		   (format nil "~S~%" (list 0 command)))))
      (write-message client (format-message command)))))

(defun check-arbiter-connection (arbiter-port)
  (unless (and *arbiter-connection*
	       (connectedp *arbiter-connection*))
    (handler-case
	(setf *arbiter-connection* 
	      (make-instance 'arbiter-connection
			     :address *default-arbiter-address*
			     :port arbiter-port
			     :connect t))
      (t (e)
	(record "unable to contact arbiter ~A" e)))))
