#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#


;;;; basic cleartext epoll connection type

(in-package :psi)

(defconstant +buffer-size+ 4096)

(defvar *connections* (make-hash-table))

(defvar *connection-graveyard* (make-hash-table))

(defclass connection ()
    ((socket :initarg :socket
           :accessor socket
           :initform nil)
     (address :initarg :address
	      :accessor address
	      :initform nil)
     (port :initarg :port
	   :accessor port
	   :initform nil)
     (poll-id :initarg :poll-id
	     :accessor poll-id
	     :initform nil)
     (incoming-message :accessor incoming-message
		       :initform (make-array 0 
					     :element-type '(unsigned-byte 8) 
					     :adjustable t))
     (outgoing-message :accessor outgoing-message
		       :initform nil)
     (message-lock :accessor message-lock
		   :initform (make-mutex))
     (outgoing-messages :accessor outgoing-messages
			:initform (make-queue))
     (event-handler :initarg :event-handler
	      :accessor event-handler
	      :initform #'handle-connection-event)
     (connected :accessor connectedp
		:initform nil))
  (:documentation "a generic connection object"))

(defgeneric disconnect-event (connection))

(defgeneric reap (connection))

(defgeneric read-message (connection message))

(defgeneric read-event (connection))

(defgeneric modify-write-flag (connection pending-write))

(defgeneric write-message (connection message))

(defgeneric write-event (connection))

(defgeneric handle-connection-event (connection event))

(defgeneric add-connection (connection &key read write))

(defmethod disconnect-event ((connection connection))
  ;; move the connection to the graveyard
  ;; and we will reap it at the end of the loop
  (setf (gethash (poll-id connection) *connection-graveyard*) connection))

(defmethod reap ((connection connection))
  (record "REAP connection")
  (let ((sock (socket connection))
	(fd (poll-id connection)))
    (remhash fd *connections*)
    (setf (connectedp connection) nil)
    (if sock
	(handler-case
	    (progn
	      (when fd (delete-watch *default-poll-controller* fd))
	      (socket-close sock))
	  (unix-error::enoent ()
	    (record "enoent on connection close"))
	  (unix-error:ebadf ()
	    (record "ebadf on connection close"))))))

(defmethod read-message ((connection connection) message)
  (record "PSI READ: ~A ~S" (length message) message))

(defmethod read-event ((connection connection))
  (multiple-value-bind (buffer length) (socket-receive (socket connection)
						       nil 
						       +buffer-size+ 
						       :element-type '(unsigned-byte 8))
    (cond 
      ((zerop length) 
       (disconnect-event connection))
      ((plusp length)
       (labels ((process-message (buffer length)
		  (let ((message (incoming-message connection))
			(index (position 0 buffer)))
		    (cond 
		      (index
		       ;; we have the end of a message in this buffer
		       (if (zerop (length message))
			     ;; this buffer contains the entire message
			     (read-message connection 
					   (octets-to-string (subseq buffer 0 index)
							     :external-format :iso-8859-1))
			   ;; we are holding earlier parts of the message
			   (progn
			     (let ((old-length (length message))
				   (new-length (+ (length message) index)))
			       (setf message (adjust-array message new-length))
			       (replace message 
					buffer 
					:start1 old-length
					:end2 index)
			       (read-message connection 
					     (octets-to-string message
							       :external-format :iso-8859-1))
			       (setf (incoming-message connection)
				     (adjust-array message 0)))))
		       (when (< index (- length 1))
			 ;; there is another message in this buffer
			 (process-message (subseq buffer (+ index 1)) 
					  (- length (+ index 1)))))
		      ((plusp length)
		       (let ((old-length (length message))
			     (new-length (+ (length message) length)))
			 ;; we haven't seen the end of the message yet,
			 ;; so just store this buffer away
			 (setf message (adjust-array message new-length))
			 (replace message 
				  buffer 
				  :start1 old-length)
			 (setf (incoming-message connection) message)))))))
	 (process-message buffer length)))
      ((minusp length)
       (record "READ-EVENT (PSI): neg return on socket-receive")
       (unless (equal (get-errno) sb-posix::eagain)
	 (disconnect-event connection))))))
    
(defmethod modify-write-flag ((connection connection) pending-write)
  (modify-watch *default-poll-controller* (poll-id connection) :read t :write pending-write))

(defmethod write-message ((connection connection) message)
  (handler-case
    (progn
      (with-mutex ((message-lock connection))
	(enqueue (string-to-octets message :external-format :iso-8859-1 :null-terminate t)
		 (outgoing-messages connection)))
      (modify-write-flag connection t))
    (unix-error:ebadf (e)
      (record "WRITE-MESSAGE (PSI) EBADF for ~A: ~A" message e)
      (disconnect-event connection)
      nil)
    (unix-error:enoent (e)
      (record "WRITE-MESSAGE (PSI) ENOENT for ~A: ~A" message e)
      (disconnect-event connection)
      nil)
    (t (e)
      (record "WRITE-MESSAGE (PSI) error for ~a: ~a" message e)
      nil)))


(defmethod write-event ((connection connection))
  (unless (outgoing-message connection)
    (with-mutex ((message-lock connection))
      (setf (outgoing-message connection) (dequeue (outgoing-messages connection)))))

  (when (outgoing-message connection)
    (let* ((length-to-send (length (outgoing-message connection)))
	   (sent-length (socket-send (socket connection)
				     (outgoing-message connection)
				     length-to-send
				     :external-format :utf-8)))
      (cond
	((= sent-length length-to-send) 
	 (with-mutex ((message-lock connection))
	   (setf (outgoing-message connection) (dequeue (outgoing-messages connection))))
	 (if (outgoing-message connection)
	     (modify-write-flag connection t)
	     (modify-write-flag connection nil)))
	((minusp sent-length)
	 (let ((err (get-errno)))
	   (unless (or (equal err sb-posix:enobufs) (equal err sb-posix:eagain))
	     (disconnect-event connection)
	     (record "unable to send message, errno ~a" err))))
	((< sent-length length-to-send)
	 (setf (outgoing-message connection)
	       (subseq (outgoing-message connection) sent-length)))))))


(defmethod handle-connection-event ((connection connection) (event event))
  (cond ((eofp event)
	 (disconnect-event connection))
	((readp event)
	 (read-event connection))
	((writep event)
	 (write-event connection))))


(define-condition connection-error (error)
  ((description :reader description
		:initarg :description
		:initform nil))
  (:report (lambda (condition stream)
	     (format stream "Connection error: ~a" (description condition)))))

