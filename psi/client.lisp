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

(defclass client-connection (connection)
  ())

(defgeneric client-connect (connection &key retry))

(defmethod initialize-instance :after ((connection client-connection) 
				       &key (connect t) (retry nil))
  (when connect
    (client-connect connection :retry retry)))

(defmethod client-connect ((connection client-connection) &key (retry nil))
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (do ()
	((connectedp connection))
      (handler-case
	  (progn
	    (socket-connect socket 
			    (address connection)
			    (port connection))
	    (setf (socket connection) socket)
	    (setf (poll-id connection) (socket-file-descriptor socket))
	    (add-connection connection))    
	(t (e)
	  (if (and retry (plusp retry))
	      (progn
		(record "client-connect error: ~a" e)
		(sleep retry))
	      (socket-error "connect")))))))

  
