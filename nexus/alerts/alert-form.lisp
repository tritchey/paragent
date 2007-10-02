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


#.(clsql:locally-enable-sql-reader-syntax)



(defgeneric render-alert-form (alert page)
  (:documentation "Wraps the form and actions around render-alert-details"))


;;; Alert forms

(defgeneric make-alert-form (alert page &key new)
  (:documentation "Makes a form that is appropriate to this alert"))

(defmethod make-alert-form ((alert alert) page &key (new nil))
  (make-alert-form (impl alert) page :new new))

(defmethod make-alert-form ((impl db::alert-impl) page &key new)
  (make-instance 'alert-form :user (user page) :alert (db-obj impl) :new-alert-p new))

(defmethod make-alert-form ((impl alert-cd) page &key new)
  (make-instance 'alert-form :user (user page) :alert (db-obj impl) :new-alert-p new))

(defmethod make-alert-form ((impl alert-software) page &key new)
  (make-instance 'alert-form :user (user page) :alert (db-obj impl) :new-alert-p new))

(defmethod make-alert-form ((impl alert-computer-offline) page &key new)
  (make-instance 'alert-form :user (user page) :alert (db-obj impl) :new-alert-p new))

(defmethod make-alert-form ((impl alert-pnp-added) page &key new)
  (make-instance 'alert-form :user (user page) :alert (db-obj impl) :new-alert-p new))

(defmethod make-alert-form ((impl alert-pnp-removed) page &key new)
  (make-instance 'alert-form :user (user page) :alert (db-obj impl) :new-alert-p new))

(defgeneric update-alert (form)
  (:documentation "Updates the db object representing the alert to reflect what has been entered
   in the form. Returns nil if the form does not validate"))


(defcomponent alert-form (paragent-component)
  ((new-alert-p :accessor new-alert-p
                :initarg :new-alert-p
                :type boolean
                :initform nil)
   (alert :accessor alert
          :type alert
          :initarg :alert)
   (note-field :accessor note-field
               :type string-field
               :initform (make-instance 'string-field :css-class "note" :value ""))
   (email-field :accessor email-field
                :type string-field
                :initform (make-instance 'string-field :css-class "emails" :value ""))
   (severity-field :accessor severity-field
                   :type alist-select-field
                   :initform (make-instance 
			      'alist-select-field
			      :test-fn #'equal
			      :data-set '(("Informational" . 0) ("Low" . 6)
					  ("Medium" . 8) ("High" . 10))))
   (computer-selector :accessor computer-selector
                      :type computer-selector)
   (event-list :accessor event-list))
  (:render ()))

(defmethod initialize-instance :after ((form alert-form) &key)
  (with-db
   (let* ((alert (alert form))
          (computers (if (slot-boundp alert 'id) (computers alert) nil))
          (selector (make-instance 'computer-selector
                                   :user (user form)
                                   :selected computers)))
     (setf (computer-selector form) selector)
     (setf (value (note-field form)) (or (note alert) ""))
     (if (new-alert-p form)
         (setf (value (email-field form)) (email (user form)))
         (setf (value (email-field form)) (email-to alert)))
     (setf (value (severity-field form)) (case (severity alert)
                                               ((0) "Informational")
                                               ((6) "Low")
                                               ((8) "Medium")
                                               ((10) "High"))))))

(defmethod render :wrapping ((form alert-form))
  (let* ((alert (alert form))
         (computers (if (slot-boundp alert 'id) (computers alert) nil))
         (selector (computer-selector form)))
    (<:fieldset
     :id "new-alert-form"
     :class "box alert-form"
     (<ucw:form
      :action (save-alert form selector computers :new (new-alert-p form))
      (<:p
       (<:label "Note:")
       (render (note-field form)))
      (<:p
       (<:label "Email:")
       (render (email-field form)))
      (<:p
       (<:label "Severity:")
       (render (severity-field form)))
      (call-next-method)
      ;; List computers this alert is applied to
      (render selector)
      (<:p
       (<ucw:input :action (save-alert form selector computers :new (new-alert-p form))
                   :class "save"
                   :type "image"
                   :src "images/savebtn.gif"))))))

(defmethod update-alert ((form alert-form))
  (if (validp form)
      (let ((alert (alert form)))
        (setf (email-to alert) (value (email-field form)))
        (setf (note alert) (value (note-field form)))
        (setf (severity alert) (value (severity-field form)))
        )
      nil))

(defmethod make-alert-form ((impl alert-hard-drive) page &key new)
  (make-instance 'alert-form-threshold :user (user page) :alert (db-obj impl) :new-alert-p new))

(defmethod make-alert-form ((impl alert-processor) page &key new)
  (make-instance 'alert-form-threshold :user (user page) :alert (db-obj impl) :new-alert-p new))

(defmethod make-alert-form ((impl alert-memory) page &key new)
  (make-instance 'alert-form-threshold :user (user page) :alert (db-obj impl) :new-alert-p new))

(defcomponent alert-form-threshold (alert-form)
  ((threshold-field 
    :accessor threshold-field
    :type integer-field
    :initform (make-instance 'integer-field
			     :validators (list
					  (make-instance 'not-empty-validator)
					  (make-instance 'number-range-validator
							 :min-value 0
							 :max-value 100)))))
  (:render 
   ()
   (<:p
    (<:label "Threshold: " (unless (validp (threshold-field alert-form-threshold))
			    (<:span :class "error" "Must be between 0 and 100")))
    (render (threshold-field alert-form-threshold)))))

(defmethod initialize-instance :after ((form alert-form-threshold) &key)
  (setf (value (threshold-field form)) (car (args (alert form)))))

(defmethod update-alert ((form alert-form-threshold))
  (call-next-method)
  (if (validp form)
      (let ((alert (alert form)))
        (setf (args alert) (list (value (threshold-field form)))))
      nil))


(defmethod make-alert-form ((impl alert-service-stop) page &key new)
  (make-instance 'alert-form-string 
		 :user (user page) :alert (db-obj impl) :field-label "Service" :new-alert-p new))

(defmethod make-alert-form ((impl alert-service-start) page &key new)
  (make-instance 'alert-form-string 
		 :user (user page) :alert (db-obj impl) :field-label "Service" :new-alert-p new))

(defmethod make-alert-form ((impl alert-process-stop) page &key new)
  (make-instance 'alert-form-string 
		 :user (user page) :alert (db-obj impl) :field-label "Process" :new-alert-p new))

(defcomponent alert-form-string (alert-form)
  ((string-field :accessor string-field
                 :type string-field
                 :initform (make-instance 'string-field
                                          :validators (list
                                                        (make-instance 'not-empty-validator))))
   (field-label :accessor field-label
                :type string
                :initform ""
                :initarg :field-label))
  (:render ()
	   (<:p
	    (<:label 
	     (<:ah (field-label alert-form-string) ": "
		   (unless (validp (string-field alert-form-string))
		     (<:span :class "error" 
			     (<:as-html (format nil "You must specify a ~(~a~)" 
						(field-label alert-form-string)))))))
	    (render (string-field alert-form-string)))))

(defmethod initialize-instance :after ((form alert-form-string) &key)
  (setf (value (string-field form)) (car (args (alert form)))))

(defmethod update-alert ((form alert-form-string))
  (call-next-method)
  (if (validp form)
      (let ((alert (alert form)))
        (setf (args alert) (list (value (string-field form)))))
      nil))


;;; Functions to actually do all the work

(defaction save-alert ((form alert-form) selector old-computers &key new)
  (when (save-alert% form selector old-computers)
    (goto-dialog-confirm form "Alert saved." (alert-link (id (alert form))))))
;    (call-component nil (make-instance 'new-alert-page-confirm))))

(defun save-alert% (form selector old-computers)
  (declare (ignore old-computers))
  (if (validp form)
      (let ((alert (alert form)))
        (with-db
          (update-alert form)
          (unless (email-to alert)
            (setf (email-to alert) ""))
          (unless (note alert)
            (setf (note alert) ""))
          (insert-and-update alert)
          (apply-alert-to-computers% form alert selector))
        t)
      nil))

(defaction delete-alert ((page paragent-component) alert)
  (%delete-alert page alert)
  (goto-alerts-page page))

(defun %delete-alert (page alert)
  (declare (ignore page))
  (with-db
    (let ((links (select 'alert-computer-link :where [= [alert-id] (id alert)] :flatp t)))
      (when links
	(delete-records :from [alert-computer-link]
	                :where [in [id] (mapcar (lambda (link)
						  (id link))
	                                        links)]))
      (dolist (alert-link links)
	(let* ((comp-id (computer-id alert-link))
	       (computer (caar (clsql:select 'computer :where [= [id] comp-id])))
	       (command (format nil "(update-alerts ~A)" comp-id)))
	  (notify-message-broker *message-broker*
	                         (lambda () (send-archon computer command))))))
      (clsql:delete-instance-records alert)))
  




(defaction apply-alert-to-computers ((page paragent-component) alert selector)
  (apply-alert-to-computers% page alert selector))

(defgeneric apply-alert-to-computers% (page alert selector))

(defmethod apply-alert-to-computers% ((page paragent-component) alert selector)
  (with-db
      (clsql:delete-records :from [alert-computer-link] :where [= [alert-id] (id alert)])
    (let ((alert-id (id alert))
          (selection (new-selected-computers selector (user page))))
      (when selection
        (clsql:execute-command
	 (format nil "INSERT INTO ALERT_COMPUTER_LINK (COMPUTER_ID, ALERT_ID) VALUES ~{(~{~a~^, ~})~^, ~}"
		 (mapcar
		  (lambda (computer)
		    (list (id computer) alert-id))
		  selection))))
      ;; update computers that are listed
      (let* ((old-computers (selected selector))
             (comp-id-hash (computers-by-guid  (append old-computers selection))))
        (maphash #'(lambda (guid id-list)
                     (notify-message-broker
		      *message-broker*
		      (lambda () (send-archon 
				  guid
				  (format nil "(update-alerts ~A)" id-list)))))
		 comp-id-hash)))))



#.(clsql:restore-sql-reader-syntax-state)
