;;;; Paragent Confidential
;;;;
;;;; Archon
;;;; (C) Copyright Paragent, LLC 2006
;;;;
;;;;
;;;; The source code for this program is not published or otherwise divested
;;;; of its trade secrets, irrespective of what has been deposited with the
;;;; U.S. Copyright office.
;;;;
;;;; All Rights Reserved. Use, duplication or disclosure restricted unless
;;;; granted by formal written contract with Paragent, LLC.

;;; this is the code used to handle the interaction with the archon instances

(in-package :nexus)

(defclass arbiter-connection (client-connection)
  ())


(defvar *arbiter-connection* nil)

(defvar *message-broker* (make-instance 'message-broker))

(defun create-message (guid command)
  (setf *print-pretty* nil)
  (if (stringp command)
      (format nil "(~S (1 ~A))~%" guid command)
      (format nil "~S~%" (list guid (list 1 command)))))

(defgeneric send-archon (computer command)
  (:documentation "Sends a command to a computer through archon"))

(defun connect-to-archon ()
  (unless (and *arbiter-connection*
	       (connectedp *arbiter-connection*))
    (start-psi-run-loop)
    (setf *arbiter-connection*
	  (make-instance 'arbiter-connection
			 :address *default-arbiter-address*
			 :port *default-nexus-arbiter-port*
			 :connect t))))

(defmethod send-archon ((guid number) command)
  (handler-case
      (progn
	(connect-to-archon)
	(if (plusp guid)
	    (write-message *arbiter-connection* (create-message guid command))
	    (record "SEND-ARCHON: no archon associated with this client")))
    (t (e)
      (setf *arbiter-connection* nil)
      (record "SEND-ARCHON: unable to contact archon: ~a" e))))

(defmethod send-archon ((computer computer) command)
  (let ((guid (archon-connection computer)))
    (if (plusp guid)
	(send-archon guid command))))

