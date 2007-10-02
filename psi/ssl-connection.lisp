#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#


;;; this is the code used to handle the interaction with the nexus instances

(in-package :psi)

(defclass ssl-connection (connection)
  ((bio :initarg :bio
	:accessor bio
	:initform nil)
   (ssl :initarg :ssl
	:accessor ssl
	:initform nil)
   (read-buffer :accessor read-buffer
		:initform (sb-alien:make-alien sb-alien:unsigned-char 
					       +buffer-size+))
   (write-buffer :accessor write-buffer
		 :initform (sb-alien:make-alien sb-alien:unsigned-char 
						+buffer-size+))
   (outgoing-message-index :accessor outgoing-message-index
			   :initform 0))
  (:documentation "an encrypted client connection")
  (:default-initargs :event-handler #'continue-accept))

(defgeneric continue-accept (connection event))

(defgeneric free-ssl-buffers (connection)
  (:documentation "we have to handle the alien buffers ourselves"))

(defmethod initialize-instance :after ((connection ssl-connection)
				       &key  
				       (address sb-bsd-sockets::inet-address-any) 
				       port 
				       (backlog *default-server-backlog*))
  (declare (ignore address port backlog))
  (let ((bio (bio-new-socket 
	      (socket-file-descriptor (socket connection)) 0)))
    (if (zerop bio)
	(error 'connection-error :description "Failed to get bio")
	(let ((ssl (ssl-new *ssl-global-context*)))
	  (if (zerop ssl)
	      (progn
		(bio-free bio)
		(error 'connection-error :description "Failed to make ssl"))
	      (progn
		(ssl-set-bio ssl bio bio)
		(setf (bio connection) bio)
		(setf (ssl connection) ssl)))))))

(defmethod free-ssl-buffers ((connection ssl-connection))
  (sb-alien:free-alien (read-buffer connection))
  (sb-alien:free-alien (write-buffer connection))
  (ssl-free (ssl connection))
  )
  

(defmethod continue-accept ((connection ssl-connection) event)
  (let* ((ssl (ssl connection))
	 (accept (ssl-accept ssl)))
    (cond 
      ((plusp accept)
       (setf (event-handler connection) #'handle-connection-event))
      ((not (let ((err (ssl-get-error ssl accept)))
	      (or (equal err +ssl-error-want-read+)
		  (equal err +ssl-error-want-write+))))
       (disconnect-event connection)))))

(defmethod disconnect-event :after ((connection ssl-connection))
  (free-ssl-buffers connection))

(defmethod read-event ((connection ssl-connection))
  (let ((num-read (ssl-read (ssl connection)
			    (read-buffer connection) 
			    +buffer-size+)))
    (cond 
      ;; there is data to be dealt with
      ((plusp num-read)
       (let* ((buffer (read-buffer connection))
	      (message (incoming-message connection))
	      (current-length (length message))
	      (new-chunk (make-array num-read :element-type '(unsigned-byte 8))))
	 (sb-kernel:copy-ub8-from-system-area (sb-alien:alien-sap buffer) 
					      0 new-chunk 0 num-read)
	 (let ((index (position 10 new-chunk)))
	   (if index
	       (if (zerop current-length)
		   (read-message connection (octets-to-string new-chunk
						      :external-format :iso-8859-1))
		   (progn
		     ;; copy to end of message and pass to read-message
		     (read-message connection 
				   (octets-to-string (concatenate '(vector (unsigned-byte 8)) 
								  message new-chunk)
						     :external-format :iso-8859-1))
		     (if (< index (1- num-read))
			 ;; there is stuff after the end of the previous message
			 (record "WARNING: we have two messages in a receive~%")
			 ;; we are done with this message
			 (setf (incoming-message connection) nil))))
	       (progn
		 ;; copy to end of message
		 (setf (incoming-message connection) (concatenate 'vector message new-chunk)) 
		 ;; call read event again since there is more of the message to read
		 (read-event connection))))))
       ((minusp num-read)
	(let ((err (ssl-get-error (ssl connection) num-read)))
	  (when (not (or (equal err +ssl-error-want-read+)
			 (equal err +ssl-error-want-write+)))
	    (disconnect-event connection))))
       ((zerop num-read)
	(disconnect-event connection)))))

(defmethod write-event ((connection ssl-connection))
  ;; first, check to see if we have a message to send
  (with-mutex ((message-lock connection))
    (unless (outgoing-message connection)
      (setf (outgoing-message connection) 
	    (dequeue (outgoing-messages connection)))))

  (if (outgoing-message connection)
    ;; outgoing-message slot. we need to keep pumping that
    (let* ((start (outgoing-message-index connection))
	   (message (outgoing-message connection))
	   (num (length message))
	   (end (if (>= (+ start +buffer-size+) num)
		    (1- num)
		    (+ start +buffer-size+)))
	   (size (- end start))
	   (buffer (write-buffer connection)))
      (if (plusp end)
	  (progn
	    (sb-kernel:copy-ub8-to-system-area message 
					       start 
					       (sb-alien:alien-sap buffer) 
					       0 
					       size)
	    (handler-case
		(let ((num-written (ssl-write (ssl connection) buffer size)))
		  (cond 
		    ((zerop num-written)
		     (disconnect-event connection))
		    ((plusp num-written)
		     (if (< (incf start num-written) (1- num))
			 (setf (outgoing-message-index connection) start)
			 (with-mutex ((message-lock connection))
			   (setf (outgoing-message-index connection) 0)
			   (unless (setf (outgoing-message connection) 
					 (dequeue (outgoing-messages 
						   connection)))
			     (modify-write-flag connection nil)))))
		    ((minusp num-written)
		     (let ((err (ssl-get-error (ssl connection) num-written)))
		       (unless (or (equal err +ssl-error-want-read+)
				   (equal err +ssl-error-want-write+))
			 (record "write-event: num-written: ~a ~% err: ~a" num-written err)
			 (disconnect-event connection))))
		    ))
	      (sb-kernel::memory-fault-error ()
		(record "PSI: memory fault while writing to client")
		(disconnect-event connection))
	      (t (e)
		 (record "write-event error: ~a" e))))))
      (modify-write-flag connection nil)))
