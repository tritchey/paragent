#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#


(in-package :arbiter)

(defvar *debug-level* 0)

(defvar *connections-by-guid* (make-hash-table :test #'equal))

(defclass message-bus-connection (connection)
  ((guid :initarg :guid
	 :accessor guid
	 :initform (get-guid)))
  (:documentation "a connection from another system component"))


(defmethod initialize-instance :after ((connection message-bus-connection)
				       &key address port backlog)
  (declare (ignore address port backlog))
  (setf (gethash (guid connection) *connections-by-guid*) connection))

(defun create-message (guid command) 
  (setf *print-pretty* nil)
  (if (stringp command)
      (format nil "(~S ~A)~%" guid command)
      (format nil "~S~%" (list guid command))))

(defun parse-message (message)
  (handler-case 
      (let* ((*read-eval* nil)
	     (message (read-from-string message))
	     (id (car message)))
	(handler-case
	    (let ((client (gethash id *connections-by-guid*))
		  (command (cadr message)))
	      (values id client command))
	  (t ()
	    (record "PARSE-MESSAGE: ERROR")
	    (values id nil nil))))
    (type-error ()
      (record "PARSE-MESSAGE: TYPE-ERROR in message: ~A" message)
      (values nil nil nil))
    (t (e)
      (record "PARSE-MESSAGE: ~a in message: ~a" e message)
      (values nil nil nil))))

(defmethod read-message ((connection message-bus-connection) message)
  ;; do a read-message from string here, pull the guid
  ;; do a search for the relavent client, and send it along
  (when (plusp *debug-level*)
    (record "READ-MESSAGE (MESSAGE-BUS-CONNECTION): ~A" message))
  (handler-case 
      (multiple-value-bind (id client command) (parse-message message)
	(if client
	    (write-message client (format nil "~S~%" command))
	    (if (and id (zerop id))
		(apply (car command) connection (cdr command))
		(record "READ-MESSAGE (MESSAGE-BUS-CONNECTION): no applicable client for message ~A" message))))
    (unix-error:ebadf ()
      (record "READ-MESSAGE: EBADF on WRITE-MESSAGE to client: ~A" message))))

(defmethod disconnect-event :after ((connection message-bus-connection))
  (remhash (guid connection) *connections-by-guid*))

