#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
s
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
		       :initform "")
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
	    (record "ebadf on connection close"))
	  (t (e)
	    (record "ERROR on connection close: ~a" e))))))

(defmethod read-message ((connection connection) message)
  (record "PSI READ: ~S" message))

(defmethod read-event ((connection connection))
  (multiple-value-bind (buffer length) (socket-receive (socket connection)
						       nil 
						       +buffer-size+ 
						       :element-type '(unsigned-byte 8))
    (cond 
      ((zerop length) 
       (disconnect-event connection))
      ((plusp length)
       ;; append new buffer onto existing
       (let ((message (incoming-message connection))
	     (*read-eval* nil))
	 (setf message (concatenate 'string message (octets-to-string buffer :external-format :iso-8859-1 :end length)))
	 (labels 
	     ((process-message (message &optional (start 0))
		(handler-case
		    (multiple-value-bind (object end) (read-from-string message t nil :start start)
		       (read-message connection object)
		       ;; is there any message left?
		       (if (< end (length message))
			    ;; there is more left - keep processing
			    (process-message message end)
			    ;; otherwise, we have grabbed the only object, 
			    ;; so just clear out and prep for next
			    (setf (incoming-message connection) nil)))
		  (end-of-file ()
		    ;; we don't have any complete objects in this string
		    (setf (incoming-message connection) (subseq message start)))
		  (t (e)
		    ;; this means something else happened, and we are f'd
		    ;; just drop the current message, and hope we re-sync
		    ;; at some point in the future
		    (record "PSI: read-event ERROR ~a~% -> ~a" e message)
		    (setf (incoming-message connection) nil)))))
	 (process-message message))))
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

