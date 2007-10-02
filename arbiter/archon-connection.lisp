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

(defvar *archon-connections* nil)

(defvar *archon-listener* nil)

(defvar *orphaned-clients* nil)

(defclass archon-connection (message-bus-connection)
  ((clients :initarg :clients
	    :accessor clients
	    :initform ()))
  (:documentation "a connection from another system component"))

(defgeneric assign-archon (client archon))

(defgeneric remove-client (client archon))

;;; when assigning clients to an archon, we want to use
;;; some strategy, like RR. This function returns the next
;;; archon-connection we should use.
;;; TODO: we can also use this oportunity to rebalance
;;; the archon-client distribution if it gets out of whack
;;; another strategy is to simply pick the archon with the least #
;;; of clients.

(let ((last-used-connection nil))
  (defun get-next-archon-connection ()
    (let ((next (cdr last-used-connection)))
      (if next
	  (progn
	    (setf last-used-connection next)
	    (car next))
	  (progn
	    (setf last-used-connection *archon-connections*)
	    (car last-used-connection))))))

(defmethod add-connection :after ((connection archon-connection) &key (read t) (write nil))
  (declare (ignore read write))
  (record "ADD-CONNECTION: Archon ~A connected" (guid connection))
  (let ((guid (guid connection)))
    (push connection *archon-connections*)
    (write-message connection (create-message 0 `(assign-guid ,guid))))
  ;; lets check to see if there are any orphaned clients, 
  ;; and take care of them
  (dolist (client *orphaned-clients*)
    (assign-archon client connection)))

(defmethod disconnect-event :after ((connection archon-connection))
  (record "DISCONNECT-EVENT: Archon ~A disconnected" (guid connection))
  (setf *archon-connections* (delete connection *archon-connections*))
  ;; we need to find another archon for all of 
  ;; the existing clients
  ;; that are currently connected
  (dolist (client (clients connection))
    (let ((next-archon (get-next-archon-connection)))
      (if next-archon
	  (assign-archon client next-archon)
	  (progn
	    (setf (archon-connection client) nil)
	    (push client *orphaned-clients*))))))

    
    
