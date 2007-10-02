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

(defaction goto-alerts-page ((page paragent-component))
  (call-component nil (make-instance 'alerts-page :user (user page))))

(defcomponent alert-list (filtered-item-list)
  ((filter :accessor filter
           :initarg :filter
           :initform nil)
   (assigned-filter :accessor assigned-filter
                    :initform nil)
   (tag-filter :accessor tag-filter
               :initform nil)
   (expanded-id :accessor expanded-id
                :initarg :expanded-id
                :initform nil
                :type integer)
   (show-checkboxes :accessor show-checkboxes
                    :initarg :show-checkboxes
                    :initform nil
                    :type boolean))
  (:documentation "Displays the company's alerts")
  (:default-initargs
    :div-id "alerts"
    :results-per-page 30
    :cellspacing 0
    :use-optimized t
    :default-view-classes (list [alerts])))

(defmethod render-title ((page alert-list))
  (<:div
    :class "box-title" :id "alerts-title"
    (<:h2 "Alerts")))

(defmethod optimized-list-count ((page alert-list))
  (car
    (select [count [distinct [slot-value 'alert 'id]]]
            :from (get-filter-tables page)
            :distinct t :flatp t
            :where [and
            [= [slot-value 'alert 'company-id] (company-id (user page))]
            (get-filter-sql page)])))
    

(defmethod optimized-list-getter ((page alert-list) start len)
  (select 'alert :order-by [slot-value 'alert 'id] :flatp t :distinct t
          :where [and
          [= [slot-value 'alert 'company-id] (company-id (user page))]
          (get-filter-sql page)]
          :offset start :limit len))

(defmethod render-item ((page alert-list) alert)
  (declare (type alert alert))
  (let* ((id (id alert))
	 (computers (computers alert))
	 (num-computers (length computers))
	 (num-events (count-alert-events alert))
         (odd-row (if (odd-row page) "odd-row" "even-row")))
    (if (not (condensed page))
	(progn
	  (<:tr
	   :class odd-row
	   (<:td :rowspan 3 :align "center" :class (format nil "badge ~a" (alert-category (type-id alert)))
		 (<:span :class "events" (<:as-html num-events))
		 (<:span :class "label" "Events"))
	   (<:td :class "name" 
		 (<:a :href (alert-link alert) (<:as-html (<:ah (description alert))))))
	  (<:tr
	   :class odd-row
	   (<:td :class "summary" (<:p
				   (<:span "Severity: ")
				   (<:span :class (format nil "~a" (alert-severity alert))
					   (<:ah (alert-severity alert)))
				   (<:span " |  Computers: ")
				   (<:ah (if (plusp num-computers)
					     num-computers
					     (<:i "(None)"))))))
	  (<:tr
	   :class (format nil "action-bar ~a" odd-row)
	   (<:td (<:ul
		  :class "filter-list"
		  (<:li :class "first-item" (<:a :href (alert-link alert) "details"))
		  (<:li (<ucw:a :action (delete-alert page alert)
				:onclick (format nil "var ret = confirm('Delete this alert?'); Effect.Puff('~a'); return ret;" id)
				"delete")))))
	  (<:tr (<:td (<:br)) (<:td)))
	(progn
	  (<:tr
	   :class odd-row
	   (<:td :rowspan 2 :align "center" :class "badge"
		 (render-icon alert))
	   (<:td :class "name" 
		 (<:span (<:as-html (description alert)))))
	  (<:tr
	   :class odd-row
	   (<:td :class "summary"
		 (<:p
		  (<:span "Events: ")
		  (if (plusp num-events)
		      (<:ah num-events)
		      (<:i"(None)"))
		  (<:span " |  Computers: ")
		  (<:ah (if (plusp num-computers)
			    num-computers
			    (<:i "(None)"))))))))))


(defmethod render-none ((page alert-list))
  (<:tr
    (<:td :colspan 0 :align "center"
          (<:h3 "No alerts found")))
  (<:tr :class "blank" (<:td :colspan 0 (<:p (<:ai "&nbsp;")))))


;;; The alerts page itself

(defcomponent alerts-page (paragent-window-component)
  ((alert-list :accessor alert-list
	       :type alert-list))
  (:default-initargs
      :title "Alerts"))

(defmethod initialize-instance :after ((page alerts-page) &key alert-id)
  (with-db
    (setf (alert-list page)
          (make-instance 'alert-list :user (user page) :expanded-id alert-id :show-checkboxes t))))

(defmethod body-id ((component alerts-page))
  "alerts")

(defrender ((page alerts-page))  
  (init-grey-box)
  (let ((user (user page)))
    (show-tip user
              "Here you can create alerts which will email you when they are triggered.
              New alerts can be created from the left side bar.
              Click on an alert to learn more about it.")
    (render (alert-list page))))

(defmethod render-tasks ((page alerts-page))
  (<:h1 "Make new alert")
  (<:ul
    :class "new-alerts"
    (<:li (<:img :src "images/computer.gif")
          (grey-box "New Computer Offline Alert" " Computer went offline"
                    (new-alert page :alert-type +alert-computer-offline+) 480 500))
    (<:li (<:img :src "images/hardware.gif")
          (grey-box "Nee Low Hard Drive Space Alert" " Low hard drive space"
                    (new-alert page :alert-type +alert-hard-drive+) 520 500))
    (<:li (<:img :src "images/hardware.gif")
          (grey-box "New Low Memory Alert" " Low memory"
                    (new-alert page :alert-type +alert-memory+) 520 500))
    (<:li (<:img :src "images/hardware.gif")
          (grey-box "New Processor Usage Alert" " Processor usage high"
                    (new-alert page :alert-type +alert-processor+) 520 500))
    (<:li (<:img :src "images/hardware.gif")
          (grey-box "New PnP Added Alert" " PnP Device Added"
                    (new-alert page :alert-type +alert-pnp-added+) 480 500))
    (<:li (<:img :src "images/hardware.gif")
          (grey-box "New Pnp Removed Alert" " PnP Device Removed"
                   (new-alert page :alert-type +alert-pnp-removed+) 480 500))
    (<:li (<:img :src "images/software.gif")
          (grey-box "New CD Inserted Alert" " CD Inserted"
                    (new-alert page :alert-type +alert-cd+) 480 500))
    (<:li (<:img :src "images/software.gif")
          (grey-box "New Software Installed Alert" " Software installed"
                    (new-alert page :alert-type +alert-software+) 480 500))
    (<:li (<:img :src "images/service.gif")
          (grey-box "New Service Started Alert" " Service started"
                    (new-alert page :alert-type +alert-service-start+) 520 500))
    (<:li (<:img :src "images/service.gif")
          (grey-box "New Service Stopped Alert" " Service stopped"
                    (new-alert page :alert-type +alert-service-stop+) 520 500))
    (<:li (<:img :src "images/service.gif")
          (grey-box "New Process Stopped Alert" " Process stopped"
                    (new-alert page :alert-type +alert-process-stop+) 520 500))
    (<:li (<:img :src "images/user.gif")
          (grey-box "New User Logged On Alert" " User logged on"
		   (new-alert page :alert-type +alert-user-logon+) 480 500))
    (<:li (<:img :src "images/user.gif")
          (grey-box "New User Logged Off Alert" " User logged off" 
		   (new-alert page :alert-type +alert-user-logoff+) 480 500))))

;;; Dialog to make a new alert

(defaction edit-alert ((page paragent-component) alert)
  (call-component nil (make-instance 'edit-alert-page :user (user page) :alert alert)))

(defaction new-alert ((page paragent-component) &key (alert-type +alert-cd+))
  (call 'new-alert-page :user (user page) :alert-type alert-type))

(defcomponent new-alert-page-confirm (simple-window-component)
  ()
  (:render ()
   (<:script
    (<:as-is
      (js:js-to-string
        ;; Cause the base window to reload when we're done
        `(setf window.onload
               (lambda ()
                 (setf parent.window.location.href ,*url-alerts*)
                 (parent.-g-b_hide))))))
   (<:p "Your alert has been created.")))


(defcomponent new-alert-page (paragent-dialog-component)
  ((alert-type :initarg :alert-type
               :accessor alert-type)
   (alert-form :accessor alert-form
               :type alert-form))
  (:default-initargs
   :title "New Alert"))

(defmethod initialize-instance :after ((page new-alert-page) &key)
  (setf (alert-form page)
        (let ((alert (make-instance 'alert
                                    :type-id (alert-type page)
                                    :company-id (company-id (user page))
                                    :email-to "")))
          (make-alert-form alert page :new t))))

(defmethod render ((page new-alert-page))
  (with-db
    (<:script :src "prototype.js")
    (<:script :src "selector.js")
    (init-computer-selector (user page))
    (<:div 
     :class "dialog"
     (<:div 
      :class "new-alert"                  
      (let ((alert-form (alert-form page)))
	(let ((alert-type (type-id (alert alert-form))))
	  (<:div
	   :id (format nil "alert-form-~a" alert-type)
	   :style (if (not (equal alert-type (slot-value page 'alert-type)))
		      "display:none;"
		      "")
	   (render alert-form))))
      (<:div :class "clear")))))



(defaction cancel-alert-creation ((page new-alert-page))
  (call 'alerts-page :user (user page)))

(defcomponent edit-alert-page (paragent-dialog-component)
  ((alert :accessor alert
	  :initarg :alert
	  :type alert)
   (alert-form :accessor alert-form
	       :type alert-form))
  (:default-initargs
   :title "Edit Alert"))

(defmethod initialize-instance :after ((page edit-alert-page) &key)
  (setf (alert-form page)
	(make-alert-form (alert page) page :new nil)))

(defmethod render ((page edit-alert-page))
  (with-db
    (<:script :src "prototype.js")
    (<:script :src "selector.js")
    (init-computer-selector (user page))
    (<:div 
     :class "dialog"
     (<:div 
      :class "new-alert"                  
      (let ((alert-form (alert-form page)))
	(render alert-form))))))

#.(clsql:restore-sql-reader-syntax-state)
