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

(defvar *network-thread* nil
  "the primary thread for the archon network stack.")

(defparameter *running* nil
  "are we running?")

(defmethod add-connection ((connection connection) &key (read t) (write nil))
  (let ((fd (socket-file-descriptor (socket connection))))
    (setf (gethash fd *connections*) connection)
    (add-watch *default-poll-controller* fd :read read :write write)
    (setf (connectedp connection) t)))

(defun close-connections ()
  (loop for connection being the hash-values of *connections* do
	(handler-case 
	    (disconnect-event connection)
	  (unix-error:ebadf ()
	    (record "CLOSE-CONNECTIONS: EBADF on connection disconnect event"))))
  (setf *connections* (make-hash-table)))
  
(defun run-loop ()
  (unwind-protect
       (do ()
	   ((not *running*))
	 (handler-case
	   (let ((events (pending-events *default-poll-controller*)))
	     ;; loop through each of the events we received
	     (loop for event across events do
		   (handler-case
		     (let* ((fd (ident event))
			    (connection (gethash fd *connections*)))
		       (when connection
			 (funcall (event-handler connection) connection event)))
		     (t (e)
			(record "run-loop error in event loop: ~a" e)))))
	   (t (e)
	      (record "run-loop error in pending-events: ~a" e))))
    (close-connections)))

(defun start-psi-run-loop ()
  (unless *running*
    (setf *running* t)
    (unless *default-poll-controller*
      (setf *default-poll-controller* (create-poll-controller)))
    (let ((s *standard-output*)
	  (p *package*)) 
      (setf *network-thread* (make-thread (lambda () (let ((*standard-output* s)
							   (*package* p))
						       (run-loop))) 
					  :name "epoll network thread")))))


(defun stop-psi-run-loop ()
  (setf *running* nil))
