#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

(in-package :nexus)

(defclass message-broker ()
  ((message :accessor message
	    :initform nil)
   (queue :accessor queue
	  :initform (make-queue))
   (lock :accessor lock
	 :initform (make-mutex))
   (message-ready :accessor message-ready
	      :initform (make-waitqueue))
   (running :accessor running-p
	    :initform t)
   (broker-thread :accessor broker-thread
		  :initform nil)))

;;; FIXME: need some kind of restart if this thread goes away

(defmethod initialize-instance :after ((broker message-broker) &key)
  (let ((s *standard-output*)
	(p *package*)) 
    (setf (broker-thread broker) (make-thread (lambda () (let ((*standard-output* s)
						      (*package* p))
						  (run-message-broker broker)))
				     :name "message-broker thread"))))

(defgeneric run-message-broker (broker))

(defmethod run-message-broker ((broker message-broker))
  (with-mutex ((lock broker))
    (while (running-p broker)
      (let ((result (dequeue (queue broker))))
	(if result
	    (progn
	      (release-mutex (lock broker))
	      (funcall result)
	      (setf result nil)
	      (get-mutex (lock broker)))
	    (condition-wait (message-ready broker) (lock broker)))))))

(defgeneric notify-message-broker (broker message))

(defmethod notify-message-broker ((broker message-broker) message)
  (with-mutex ((lock broker))
    (enqueue message (queue broker))
    (condition-notify (message-ready broker))))
  