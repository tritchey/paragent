#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#


;;; functions for monitoring the running templar instances we know about

(in-package :archon)

#.(clsql:locally-enable-sql-reader-syntax)

(defvar *heartbeat-timeout* 300)

(defun mark-all-computers-offline-but (good-computers) 
  (db:with-db
      (let* ((online-computers (clsql:select [id] 
					     :from [computers] 
					     :where [and [= [online] 1]
							 [= [archon_connection] 
							    *templar-connection-string*]]
					     :refresh t))
	     (online-ids (set-difference (mapcar (lambda (item) (car item)) 
						 online-computers)
					 good-computers)))
	(dolist (id online-ids)
	  (let ((computer (clsql:select 'computer :where [= [id] id] :flatp t)))
	    (when (car computer)
	      (record-offline-event (car computer))))))))

(defun mark-all-computers-online ()
  (db:with-db
    (clsql:update-records [computers] :av-pairs '((online 1)) 
			  :where [= [archon_connection] *templar-connection-string*])))

(defun mark-all-computers-offline ()
  (db:with-db
    (clsql:update-records [computers] :av-pairs '((online 0)) 
			  :where [= [archon_connection] *templar-connection-string*])))

(defun requery-old-clients ()
  (let* ((one-day (clsql:time- (clsql:get-time) (clsql:make-duration :day 1)))
	 (computers (db:with-db 
			(clsql:select 'computer :where [< [last-online] one-day]))))
    (loop for c in computers do
	  (let ((client (gethash (id (car c)) *clients-by-computer-id*)))
	    (when client
	      (setf (old-computer-p client) t)
	      (query-all-props client))))))


(defun check-heartbeats ()
  (let ((good-computers '()))
    (loop for client being the hash-values of *clients-by-guid* do
	  (if (heartbeatp client)
	      (progn
		(setf (heartbeatp client) nil)
		(setf good-computers (push (computer-id client) good-computers)))
	      (progn
		(record "failed hearbeat check for ~A" (name client))
		(when (computer-id client)
		  (record-offline-event (computer client)))
		(disconnect-client client))))
    (mark-all-computers-offline-but good-computers))
  (requery-old-clients))

#.(clsql:restore-sql-reader-syntax-state)
	

