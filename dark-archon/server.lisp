#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

;;; this is the code for handling the http connections from the js-based vnc client
(in-package :dark-archon)


(defclass observer-connection ()
    ((probe-connection :accessor probe-connection
		       :initform nil)
     (message-lock :accessor message-lock
		   :initform (make-mutex))
     (lock :accessor lock
	   :initform (make-mutex))
     (message-ready :accessor message-ready
		    :initform (make-waitqueue))
     (outgoing-messages :accessor outgoing-messages
			:initform (make-queue)))
  (:documentation "a generic connection object"))

(defparameter *global-connection* (make-instance 'observer-connection))

(defvar *this-file* (load-time-value
                     (or #.*compile-file-pathname* *load-pathname*)))

(defmacro with-html (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     ,@body))

(defun vnc-session ()
  (let ((response (raw-post-data)))
    (when (and response (probe-connection *global-connection*))
      (write-message (probe-connection *global-connection*) response)))

  (no-cache)
  ;; block here until we have something to send
  ;; ?? what happens if the call times out?
  (handler-case 
      (let ((connection *global-connection*)
	    (message nil))
	(with-mutex ((message-lock connection))
	  (while (not message)
	    (setf message (dequeue (outgoing-messages connection)))
	    (record "~A" message)
	    (unless message
		(condition-wait (message-ready connection) (message-lock connection)))))
	message)
    (t (e) (format t "error: ~A~%" e))))

(defun menu ()
  (with-html
    (:html
     (:head
      (:title "DARK-ARCHON")
      (:script :src "/da/jquery.js" :type "text/javascript")
      (:script :src "/da/binary.js" :type "text/javascript")
      (:script :src "/da/vnc.js" :type "text/javascript"))
     (:body
      (:h2 "DARK-ARCHON server")
      (:a :href "#" :onclick "javascript: connect()" "connect")
      (:canvas :id "canvas" :width "1024" :height "768")
      (:ul :id "console"
	    (:p "Console:"))))))

(setq *dispatch-table*
      (nconc
       (list 'dispatch-easy-handlers
             (create-static-file-dispatcher-and-handler
              "/da/jquery.js"
              (make-pathname :name "jquery" 
			     :type "js" 
			     :version nil
			     :defaults "/Users/tritchey/Projects/Paragent/git/dark-archon/")
              "text/javascript")
             (create-static-file-dispatcher-and-handler
              "/da/binary.js"
              (make-pathname :name "binary" 
			     :type "js" 
			     :version nil
			     :defaults "/Users/tritchey/Projects/Paragent/git/dark-archon/")
              "text/javascript")
             (create-static-file-dispatcher-and-handler
              "/da/vnc.js"
              (make-pathname :name "vnc" 
			     :type "js" 
			     :version nil
			     :defaults "/Users/tritchey/Projects/Paragent/git/dark-archon/")
              "text/javascript"))
	     (mapcar (lambda (args)
                 (apply #'create-prefix-dispatcher args))
               '(("/da" menu)
		 ("/vnc" vnc-session)))
       (list #'default-dispatcher)))
