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

(in-package :dark-archon)

(defvar *dark-templars* (make-hash-table :test #'equal))
(defvar *observers* (make-hash-table :test #'equal))
(defconstant +buffer-size+ 8192)

(defclass probe-connection (connection)
  ((identified :accessor identifiedp
	       :initform nil)
   (table :accessor table
	  :initform nil)
   (key :accessor key
	:initform nil)
   (target :accessor target
	   :initform nil)
   (type :accessor type
	 :initform "")))

(defgeneric identify (connection message))

(defmethod identify ((connection probe-connection) message)
  (handler-case
      (let* ((*read-eval* nil)
	     (possible-ident (read-from-string message)))
	(record "identifying connection: ~S~%" possible-ident)
	(cond 
	  ((equal (first possible-ident) "DARK-TEMPLAR")
	   (let ((key (list (second possible-ident) 
			    (third possible-ident))))
	     (record "dark-templar: ~A~%" key)
	     
	     ;; check to make sure we don't have a duplicate
	     (let ((existing-connection (gethash key *dark-templars*)))
	       (when existing-connection
		 (disconnect-event existing-connection)))
	     (progn
	       (setf (identifiedp connection) t)
	       (setf (gethash key *dark-templars*) connection)
	       (setf (table connection) *dark-templars*)
	       (setf (key connection) key)
	       (setf (type connection) "DARK-TEMPLAR")
	       ;; link up this connection if the observer is already connected
	       (let ((target (gethash key *observers*)))
		 (when target
		   (setf (target connection) target)
		   (setf (target target) connection)
		   (sleep 2)
		   (write-message connection (format nil "(GOOD)~%"))
		   (write-message target (format nil "(GOOD)~%")))))))
	  ((equal (first possible-ident) "OBSERVER")
	   (let ((key (list (second possible-ident) 
			    (third possible-ident))))
	     (record "Observer: ~A~%" key)
	     ;; check that it isn't a dupe
	     (let ((existing-connection (gethash key *observers*)))
	       (when existing-connection
		 (disconnect-event existing-connection)))
	     (progn
	       (setf (identifiedp connection) t)
	       (setf (gethash key *observers*) connection)
	       (setf (table connection) *observers*)
	       (setf (key connection) key)
	       (setf (type connection) "OBSERVER")
	       ;; link up this connection if dark-templar is already connected
	       (let ((target (gethash key *dark-templars*)))
		 (when target
		   (setf (target target) connection)
		   (setf (target connection) target)
		   (write-message target (format nil "(GOOD)~%"))
		   (write-message connection (format nil "(GOOD)~%")))))))
	  (t
	   (record "bad connection attempt ~a~%")
	   (write-message connection (format nil "(FAIL)~%")))))
    (end-of-file (e)
      (record "IDENTIFY: end-of-file ~a on message ~a~%" e message))
    (undefined-function (e)
      (record "IDENTIFY: undefined-function ~a in ~a~%" e message))
    (type-error (e)
      (record "IDENTIFY: type-error ~a on message ~a~%" e message))
    (t (e)
      (record "IDENTIFY: error ~a on message ~a~%" e message))))

(defmethod write-message ((connection probe-connection) message)
  (handler-case
    (progn
      (with-mutex ((message-lock connection))
	(enqueue (string-to-octets message :external-format :iso-8859-1)
		 (outgoing-messages connection)))
      (modify-write-flag connection t))
    (unix-error:ebadf (e)
      (record "WRITE-MESSAGE (PSI) EBADF for ~A: ~A" message e)
      (disconnect-event connection))
    (t (e)
      (record "WRITE-MESSAGE (PSI) error for ~a: ~a" message e))))

(defmethod read-message ((connection probe-connection) message)
  (handler-case
      (if (and (identifiedp connection) (target connection))
	  (write-message (target connection) message)
	  (identify connection message))
    (end-of-file (e)
      (record "READ-MESSAGE: end-of-file ~a on message ~A~%" e message))
    (undefined-function (e)
      (record "READ-MESSAGE: undefined-function ~a in ~A~%" e message))
    (type-error (e)
      (record "READ-MESSAGE: type-error ~a on message ~A~%" e message))
    (t (e)
      (record "READ-MESSAGE: unknown error: ~a for message ~a" e message))))

(defmethod read-event ((connection probe-connection))
  (multiple-value-bind (buffer num-read) 
      (socket-receive (socket connection) nil +buffer-size+ :element-type '(unsigned-byte 8))
    (cond 
      ((plusp num-read)
       (read-message connection (octets-to-string (subseq buffer 0 num-read) :external-format :iso-8859-1)))
      ((zerop num-read)
       (disconnect-event connection)))))

(defmethod reap :after ((connection probe-connection))
  (record "disconnecting ~a ~a" (type connection) (key connection))
  (let ((target (target connection)))
    (when (table connection)
      (remhash (key connection) (table connection)))
    (when (and target (connectedp target))
      (disconnect-event target))))
