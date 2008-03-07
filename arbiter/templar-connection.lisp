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

(defvar *templar-listener* nil)

(defun get-guid ()
  ;; in the extreme unlikelihood that we have generated an identical guid
  ;; call get-guid again. This should recurse until we DO have a unique id
  (let ((guid (make-guid)))
    (if (gethash guid *connections-by-guid*)
	(get-guid)
	guid)))

(defclass templar-connection (ssl-connection)
  ((guid :initarg :guid
	 :accessor guid
	 :initform (get-guid))
   (archon-connection :initarg archon-connection
		      :accessor archon-connection
		      :initform nil))
  (:documentation "a connection from a templar agent"))

(defun disconnect-client (archon-connection id)
  (declare (ignore archon-connection))
  (let ((connection (gethash id *connections-by-guid*)))
    (when connection
      (disconnect-event connection))))


(defmethod add-connection :after ((connection templar-connection) &key read write)
  (declare (ignore read write))
  (setf (gethash (guid connection) *connections-by-guid*) connection)
  (let ((next-archon (get-next-archon-connection)))
    (if next-archon
	(assign-archon connection next-archon)
	(push connection *orphaned-clients*))))
  
(defmethod read-message ((connection templar-connection) message)
  (when (plusp *debug-level*)
    (record "READ-MESSAGE (TEMPLAR-CONNECTION): ~A" message))
  (handler-case
      (when (archon-connection connection)
	(let ((message (create-message (guid connection) message)))
	  (write-message (archon-connection connection) message)))
    (end-of-file ()
      (format t "ARBITER: end-of-file on message ~A~%" message))
    (undefined-function ()
      (format t "ARBITER: undefined-function in ~A~%" message))
    (type-error ()
      (format t "ARBITER: type-error on message ~A~%" message))
    (t (e)
      (format t "ARBITER: ~A sending: ~A~%" e message))))

(defmethod disconnect-event :after ((connection templar-connection))
  (remhash (guid connection) *connections-by-guid*)
  (let ((archon (archon-connection connection)))
    (when archon
      (remove-client connection archon))))

(defmethod assign-archon ((client templar-connection) (archon archon-connection))
  (setf (archon-connection client) archon)
  (push client (clients archon))
  ;; let archon know about this new connection
  (write-message archon (create-message (guid client) '(connected))))
      
  
(defmethod remove-client ((client templar-connection) (archon archon-connection))
  (setf (clients archon) (remove client (clients archon)))
  (write-message archon
		 (create-message (guid client)
				 '(0 (disconnected)))))

