#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#


(in-package :psi)

(defparameter *default-server-backlog* 16
  "The default number of simultaneous connections to the server.")

(defclass server-connection (connection)
  ((client-class :initarg :client-class
		 :accessor client-class
		 :initform 'connection))
  (:default-initargs :event-handler #'accept-connection))

(defgeneric accept-connection (connection event))

(defmethod accept-connection ((connection server-connection) (event event))
  (multiple-value-bind (socket address port) (socket-accept (socket connection))
    (setf (non-blocking-mode socket) t)
    (handler-case
      (let* ((fd (socket-file-descriptor socket))
	     (connection (make-instance (client-class connection)
					:socket socket
					:address address
					:port port
					:poll-id fd)))
	(add-connection connection))
      (connection-error (e)
	 (record "accept-connection failed: ~a" e))
      (t (e)
	 (record "accept-connection error: ~a" e)))))


(defmethod initialize-instance :after ((connection server-connection) 
     &key  (address sb-bsd-sockets::inet-address-any) port (backlog *default-server-backlog*))
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (setf (sockopt-reuse-address socket) t)
    (socket-bind socket address port)
    (socket-listen socket backlog)
    (setf (socket connection) socket)
    (add-connection connection)))

				   