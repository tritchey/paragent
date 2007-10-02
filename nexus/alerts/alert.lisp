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

(defun alert-name (alert-type-id)
  (declare (integer alert-type-id))
  (case alert-type-id
        (#.+alert-cd+ "CD Inserted")
        (#.+alert-computer-offline+ "Computer went offline")
        (#.+alert-hard-drive+ "Low hard drive space")
        (#.+alert-memory+ "Low memory")
        (#.+alert-software+ "Software installed")
        (#.+alert-service-start+ "Service started")
        (#.+alert-service-stop+ "Serviced stopped")
        (#.+alert-process-stop+ "Process stopped")
        (#.+alert-processor+ "Processor usage high")
        (#.+alert-pnp-added+ "Plug-and-play device added")
        (#.+alert-pnp-removed+ "Plug-and-play device removed")
	(#.+alert-user-logon+ "User Logged On")
	(#.+alert-user-logoff+ "User Logged Off")))

(defun alert-category (alert-type-id)
  (declare (integer alert-type-id))
  (case alert-type-id
        (#.+alert-cd+ "software")
        (#.+alert-computer-offline+ "computer")
        (#.+alert-hard-drive+ "hardware")
        (#.+alert-memory+ "hardware")
        (#.+alert-software+ "software")
        (#.+alert-service-start+ "service")
        (#.+alert-service-stop+ "service")
        (#.+alert-process-stop+ "service")
        (#.+alert-processor+ "hardware")
        (#.+alert-pnp-added+ "hardware")
        (#.+alert-pnp-removed+ "hardware")
	(#.+alert-user-logon+ "user")
	(#.+alert-user-logoff+ "user")))



(defun alert-severity (alert)
  (case (severity alert)
    (0 "Informational")
    (6 "Low")
    (8 "Medium")
    (10 "High")
    (otherwise "Note")))

(defun count-alert-events (alert)
  (car
   (with-db
    (select [count [distinct [id]]]
            :from [alert-event-link]
            :distinct t :flatp t
            :where [= [alert-id] (id alert)]))))

;; Dialog to associate a computer with a ticket

(defaction open-computer-alert-dialog ((page paragent-component) alert)
  (call-component nil (make-instance 'computer-alert-dialog :alert alert :user (user page))))

(defcomponent computer-alert-dialog (paragent-dialog-component)
  ((alert :accessor alert
          :initarg :alert
          :type alert)
   (computer-selector :accessor computer-selector
                      :type computer-selector))
  (:documentation "This dialog exists to assign alerts to computers.")
  (:default-initargs
    :title "Add/Remove Computers"))

(defmethod initialize-instance :after ((page computer-alert-dialog) &rest rest)
  (declare (ignore rest))
  (with-db
    (setf (computer-selector page)
          (make-instance 'computer-selector :selected (computers (alert page)) :user (user page)))))

(defmethod render ((page computer-alert-dialog))
  (with-db
    (<:script :src "prototype.js" :type "text/javascript")
    (init-computer-selector (user page))
    (<ucw:form
     :action (set-computers page)
     (<:fieldset
      :id "add-remove-computer"
      :class "box"
        (render (computer-selector page))
        (<ucw:submit :action (apply-alert-to-computers page (alert page) (computer-selector page))
		     :class "login"
		     :type "image"
		     :src "images/savebtn.gif")))))


(defaction apply-alert-to-computers ((page computer-alert-dialog) alert selector)
  (apply-alert-to-computers% page alert selector)
  (call 'dialog-confirm :redirect-to (alert-link alert)))

;;; render alert helper function

(defgeneric render-alert (page alert alert-event-list alert-form)
  (:documentation "Renders the alert in a pretty, non-editable fashion"))

(defmethod render-alert (page (alert alert) alert-event-list alert-form)
  (render-alert page (impl alert) alert-event-list alert-form))

(defmethod render-alert :around (page (impl db::alert-impl) alert-event-list form)
  (let* ((alert (db-obj impl))
         (computers (computers alert))
         (num-computers (length computers))
         (alert-id (id alert))
	 (num-events (count-alert-events alert))
         (id (unique-id "alert")))
    (<ucw:form
     :action (save-alert form (computer-selector form) computers)
     :id id
     (<:table
      :id "alerts"
      :class "item-list"
      :cellspacing 5
      (<:tr
       (<:td :rowspan 6 :align "center" :class (format nil "badge ~a" (alert-category (type-id alert)))
             (<:span :class "events" (<:as-html num-events))
	     (<:span :class "label" "Events"))
       (<:td :class "name"
             :colspan 2
             (<:ah (description alert))))
      (<:tr
       (<:td :class "title" (<:p "Emails"))
       (<:td :class "desc"
	     (<:span :id (format nil "alert-email~a" alert-id)
                     (<:ah (if (not-blank (email-to alert))
                               (email-to alert)
                               (<:i "(None)"))))
             (<:span :id (format nil "alert-email-e~a" alert-id) :style "display:none;"
                     (render (email-field form)))))
      (<:tr
       (<:td :class "title" (<:p "Note"))
       (<:td :class "desc"
	     (<:span :id (format nil "alert-note~a" alert-id)
                     (<:ah (if (not-blank (note alert))
                               (note alert)
                               (<:i "(None)"))))
             (<:span :id (format nil "alert-note-e~a" alert-id) :style "display:none;"
                     (render (note-field form)))))
      (<:tr
       (<:td :class "title" (<:p "Severity"))
       (<:td :class "desc"
	     (<:span :id (format nil "alert-severity~a" alert-id)
                     (<:ah (alert-severity alert)))
             (<:span :id (format nil "alert-severity-e~a" alert-id) :style "display:none;"
                     (render (severity-field form)))))
      (call-next-method)
      (<:tr
       :class "action-bar"
       (<:td
        :colspan 2
        (<:ul
         :class "action-list"
         (let* ((names (mapcar (lambda (name) (list (format nil "~a~a" name alert-id)
                                                    (format nil "~a-e~a" name alert-id)))
                               '("alert-email" "alert-note" "alert-severity" "alert-extra" "save-alert")))
                (edit-js (format nil "javascript: ~a"
                                (js:js-to-string
                                 `(progn
                                   ,@(mapcar
                                      (lambda (name)
                                        `(progn
                                          (.hide -element ,(first name))
                                          (.-appear -effect ,(second name))))
                                      names)
                                   (return false)))))
                (unedit-js (format nil "javascript: ~a"
                                (js:js-to-string
                                 `(progn
                                   ,@(mapcar
                                      (lambda (name)
                                        `(progn
                                          (.hide -element ,(second name))
                                          (.-appear -effect ,(first name))))
                                      names)
                                      (return false))))))
           (<:li 
            :class "first-item" :id (format nil "save-alert~a" alert-id)
            (<ucw:a :action (edit-alert page alert)
                    :onclick edit-js
                    "edit"))
           (<:span :id (format nil "save-alert-e~a" alert-id) :style "display: none;"
                   (<:li :class "first-item"
                    (<:a 
		     :href "#"
		     :onclick (format nil "javascript:$('~a').submit();" id)
		     "save"))
                   (<:li
                    (<:a :href "#" :onclick unedit-js "cancel")))
            (<:li
             (<ucw:a :action (delete-alert page alert)
                     :onclick (format nil "var ret = confirm('Delete this alert?'); Effect.Puff('~a'); return ret;" id)
                     "delete"))))))))))
      
(defmethod render-alert (page (impl db::alert-impl) alert-event-list alert-form)
  (<:span :id (format nil "alert-extra~a" (id (db-obj impl))) :style "display:none;")
  (<:span :id (format nil "alert-extra-e~a" (id (db-obj impl))) :style "display:none;"))

(defmethod render-alert (page (impl db::alert-1arg) alert-event-list alert-form)
  (<:tr
    (<:td :class "title" (<:p "Threshold"))
    (<:td :class "desc" (<:span :id (format nil "alert-extra~a" (id (db-obj impl)))
				(<:ah (arg1 impl) "%"))
          (<:span :class "threshold" :id (format nil "alert-extra-e~a" (id (db-obj impl))) :style "display:none;"
                  (render (threshold-field alert-form)) (<:ah "%")))))

(defmethod render-alert (page (impl alert-service-start) alert-event-list alert-form)
  (<:tr
    (<:td :class "title" (<:p "Service"))
    (<:td :class "desc" (<:span :id (format nil "alert-extra~a" (id (db-obj impl)))
				(<:ah (arg1 impl)))
          (<:span :class "service" :id (format nil "alert-extra-e~a" (id (db-obj impl))) :style "display:none;"
                  (render (string-field alert-form))))))

(defmethod render-alert (page (impl alert-service-stop) alert-event-list alert-form)
  (<:tr
    (<:td :class "title" (<:p "Service"))
    (<:td :class "desc" (<:span :id (format nil "alert-extra~a" (id (db-obj impl)))
				(<:ah (arg1 impl)))
          (<:span :class "service" :id (format nil "alert-extra-e~a" (id (db-obj impl))) :style "display:none;"
                  (render (string-field alert-form))))))

(defmethod render-alert (page (impl alert-process-stop) alert-event-list alert-form)
  (<:tr
    (<:td :class "title" (<:p "Process"))
    (<:td :class "desc" (<:span :id (format nil "alert-extra~a" (id (db-obj impl)))
                  (<:ah (arg1 impl)))
          (<:span :class "process" :id (format nil "alert-extra-e~a" (id (db-obj impl))) :style "display:none;"
                  (render (string-field alert-form))))))


(defgeneric render-icon (alert)
  (:documentation "Renders an image which represents this alert"))

(defmethod render-icon ((alert alert))
  (render-icon (impl alert)))

(defmethod render-icon ((alert alert-computer-offline))
  (<:img :src "images/computer.gif"))

(defmethod render-icon ((alert alert-hard-drive))
  (<:img :src "images/hardware.gif"))

(defmethod render-icon ((alert alert-software))
  (<:img :src "images/software.gif"))

(defmethod render-icon ((alert alert-processor))
  (<:img :src "images/hardware.gif"))

(defmethod render-icon ((alert alert-cd))
  (<:img :src "images/software.gif"))

(defmethod render-icon ((alert alert-memory))
  (<:img :src "images/hardware.gif"))

(defmethod render-icon ((alert alert-service-start))
  (<:img :src "images/software.gif"))

(defmethod render-icon ((alert alert-service-stop))
  (<:img :src "images/software.gif"))

(defmethod render-icon ((alert alert-process-stop))
  (<:img :src "images/software.gif"))

(defmethod render-icon ((alert alert-pnp-added))
  (<:img :src "images/hardware.gif"))

(defmethod render-icon ((alert alert-pnp-removed))
  (<:img :src "images/hardware.gif"))

(defmethod render-icon ((alert alert-user-logon))
  (<:img :src "images/user.gif"))

(defmethod render-icon ((alert alert-user-logoff))
  (<:img :src "images/user.gif"))


;;; alert-tabbed-view
;;; the component that provides the body and tabs of the page

(defcomponent alert-tabbed-view (tabbed-view)
  ((alert :accessor alert
          :initarg :alert
          :type alert)
   (computers :accessor computers
	      :initarg :computers
              :type list)
   (alert-form :accessor alert-form
               :type alert-form)
   (alert-event-list :accessor alert-event-list
                     :type inline-event-list))
  (:default-initargs
    :toolbar #'render-alert-info
    :contents (list #'render-alert-events
                    #'render-alert-computers)
    :tabs (list "Events" "Computers")))

(defgeneric render-alert-events (page))

(defgeneric render-alert-computers (page))

(defgeneric render-alert-info (page))

(defmethod initialize-instance :after ((page alert-tabbed-view) &key)
  (with-db
      (let ((alert (alert page))
	    (user (user page)))
	(setf (computers page) (computers alert))
	(setf (alert-event-list page) (make-instance 'inline-event-list 
						     :condensed t 
						     :results-per-page 15
						     :user user))
	(set-filter (alert-event-list page) 
		    :alert
		    [and
		    [= [slot-value 'event 'id] [slot-value 'alert-event-link 'event-id]]
		    [= [slot-value 'alert-event-link 'alert-id] (id (alert page))]]
		    [alert-event-link])
	(setf (alert-form page) (make-alert-form alert page :new nil))
	(let ((len (length (computers page))))
	  (setf (tabs page) (list "Events"
				  (format nil (if (equal len 1) "~a Computer" "~a Computers")
					  len)))))))

(defmethod render-alert-events ((page alert-tabbed-view))
  (render (alert-event-list page)))

(defmethod render-alert-computers ((page alert-tabbed-view))
  (let ((computers (computers page))
	(alert (alert page)))
    (<:h2 "Computers ")
    (<:ul :class "filter-list"
	  (<:li :class "select-all first-item"
		(grey-box "Add/Remove Computers" "Add/Remove"
			  (open-computer-alert-dialog page alert) 320 500)))
    (if computers
	(<:ul
	 (dolist (computer computers)
	   (let ((computer computer)) ; rebinding for the actions to work
	     (<:li
	      (<:img :width "24px" 
		     :height "24px"
		     :style "vertical-align: middle;"
		     :src "images/computer.gif")
	      " "
	      (<:a :href (computer-link computer)
		   (<:ah (name computer)))))))
	(<:p "This alert has not been applied to any computers. "
           (grey-box "Edit Alert" "Click here" (edit-alert page (alert page)) 515 500)
           (<:ah " to do so.")))))

(defmethod render-alert-info ((page alert-tabbed-view))
  (render-alert page (alert page) (alert-event-list page) (alert-form page)))


;;; alert-page
;;; the main window component for a single alert

(defcomponent alert-page (paragent-window-component)
  ((alert :accessor alert
	  :initarg :alert
          :type alert)
   (tabbed-view :accessor tabbed-view
		:initarg tabbed-view
		:type alert-tabbed-view)))

(defmethod initialize-instance :after ((page alert-page) &key title)
  (declare (ignore title))
  (setf (tabbed-view page)
	(make-instance 'alert-tabbed-view 
		       :alert (alert page) 
		       :user (user page)
	               :place (make-place (tabbed-view page)))))

(defrender ((page alert-page))
    (<:script :src "actb/actb.js" :type "text/javascript")
  (<:script :src "actb/common.js" :type "text/javascript")
  (init-grey-box)
  (render (tabbed-view page)))

(defmethod body-id ((component alert-page))
  "alerts")

(defun alert-for-id (alert-id company-id)
  (declare (integer alert-id company-id))
  (with-db 
    (car (select 'alert 
		 :flatp t 
		 :limit 1
		 :where [and [= [id] alert-id]
                             [= [company-id] company-id]]))))

(defentry-point "alert.ucw" (:application *my-app*)
  (id)
  (let ((user (get-user))
	(alert-id (parse-integer id :junk-allowed t)))
    (if user
        (let ((alert (alert-for-id alert-id (company-id user))))
          (if alert
              (call 'alert-page :user user :alert alert :title (description alert))
              (call 'redirect-component :target "alerts.ucw")))
        (call 'redirect-component :target "login.ucw"))))

(defmethod alert-link ((alert alert))
  (format nil "alert.ucw?id=~a" (id alert)))

(defmethod alert-link ((id integer))
  (format nil "alert.ucw?id=~a" id))



#.(clsql:restore-sql-reader-syntax-state)
