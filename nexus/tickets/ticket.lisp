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

(defentry-point "ticket.ucw" (:application *my-app*)
  (id)
  (let ((user (get-user))
        (ticket-id (parse-integer id :junk-allowed t)))
    (if (and user ticket-id)
        (let ((ticket (ticket-for-id ticket-id (company-id user))))
          (if ticket
              (call 'ticket-page :user user :ticket ticket)
              (call 'redirect-component :target "tickets.ucw")))
        (call 'redirect-component :target "login.ucw"))))

(defun ticket-for-id (ticket-id company-id)
  (declare (integer ticket-id company-id))
  (with-db 
    (car (select 'ticket :flatp t :limit 1
                 :where [and 
                 [= [id] ticket-id]
                 [= [company-id] company-id]]))))

(defgeneric render-ticket-toolbar (page)
  (:documentation "render the toolbar for the ticket details page"))

(defgeneric render-ticket-history-list (page)
  (:documentation "Shows all the history for a ticket"))

(defgeneric render-ticket-computer-list (page)
  (:documentation "Show all the computers for a ticket"))


(defcomponent ticket-tabbed-view (tabbed-view)
  ((ticket :accessor ticket
	   :initarg :ticket
	   :type ticket)
   (parent :accessor parent
	   :initarg :parent
	   :initform nil))
  (:default-initargs
      :toolbar #'render-ticket-toolbar
      :contents (list #'render-ticket-history-list
		      #'render-ticket-computer-list)
      :tabs (list "History" "Computers")))

(defmethod initialize-instance :after ((page ticket-tabbed-view) &key)
  (with-db
      (let* ((ticket (ticket page))
	     (len (length (computers ticket))))
	(setf (tabs page) (list "History"
				(format nil (if (equal len 1) "~a Computer" "~a Computers")
					len))))))

(defcomponent ticket-page (paragent-window-component)
  ((ticket :accessor ticket
           :initarg :ticket
           :type ticket)
   (tabbed-view :accessor tabbed-view
		:initarg tabbed-view
		:type ticket-tabbed-view)
   (error-message :accessor error-message
		  :initarg :error-message
		  :initform nil)))

(defmethod initialize-instance :after ((page ticket-page) &key)
  (let ((ticket (ticket page)))
    (setf (window-component.title page) (format nil "~a - ~a" (ticket-id ticket) (subject ticket)))
    (setf (tabbed-view page)
          (make-instance 'ticket-tabbed-view 
                         :ticket (ticket page) 
                         :user (user page)
			 :parent page
                         :place (make-place (tabbed-view page))))))


(defmethod body-id ((component ticket-page))
  "tickets")

(defmethod render-tasks ((page ticket-page))
  (with-db
   (let* ((user (user page))
          (ticket (ticket page))
          (recent (sort (recent-tickets user) (lambda (x y) (> (id x) (id y))))))
     (<:h1 "Tickets")
     (<:ul
      (<:li (grey-box "New Ticket" "New Ticket..."
                      (new-ticket page) 439 530))
      (when (>=admin (user page))
        (<:li (grey-box "Watch this email address..." "Add email account to monitor..."
                        (goto-ticket-email-dialog page) 170 360))))
     (let ((old (find-if (lambda (x) (equal (ticket-id x) (id ticket))) recent))
           (new (make-instance 'recent-ticket :user-id (id user) :ticket-id (id ticket))))
       (if old
           (progn
             (delete-records :from [recent-tickets] :where [= [id] (id old)])
             (setf recent (cons new (remove old recent)))
             (update-records-from-instance new))
           (progn
             (update-records-from-instance new)
             (setf recent (cons new recent))
             (when (> (length recent) 10)
               (let ((last (car (last recent))))
                 (delete-records :from [recent-tickets] :where [= [id] (id last)])
                 (setf recent (remove last recent)))))))
     (<:h1 "Recent Tickets")
     (<:ul
      (<:li (<:a :href *url-tickets* "All Tickets"))
      (dolist (recent-ticket recent)
        (let ((ticket (ticket recent-ticket)))
          (when ticket
            (<:li (<:a :href (ticket-link ticket)
                       (<:ah (ticket-id ticket) " - " (subject ticket)))))))))))

(defrender ((page ticket-page))
  (<:script :src "actb/actb.js" :type "text/javascript")
  (<:script :src "actb/common.js" :type "text/javascript")
  (init-grey-box)
  (render (tabbed-view page))
  (when (error-message page)
    (<:script :type "text/javascript"
	      (<:ai (format nil "alert(\"~a\")" (error-message page))))))


(defmethod render-ticket-toolbar ((page ticket-tabbed-view))
  (let* ((ticket (ticket page))
         (user (user page))
	 (class (if (equal (state ticket) +ticket-status-closed+)
		    "Closed"
		    (db::priority-name ticket)))
         (ticket-form (make-instance 'ticket-form :ticket ticket :user user)))
    (<ucw:form
     :id "edit-ticket-details"
     :action (save-ticket ticket-form)
     :method "post"
     (<:table :cellspacing 5
      :id "tickets"
      (<:tr
       (<:td :rowspan 10 :align "center" :class (format nil "badge ~a" class)
	     (<:p (<:span :class "state" (<:as-html (state-string ticket)))))
       (<:td :class "name" :colspan 2
	     (<:span (<:as-html (ticket-id ticket)) 
		     (<:as-is " &ndash; ")
	             (<:ah (subject ticket)))))
      (<:tr
       (<:td :class "title"
	     (<:p "Created"))
       (<:td :class "desc"
	     (<:ah (if (timestamp ticket) 
		       (adjusted-timestamp ticket user) 
		       (<:i "(Missing Timestamp)")))))
      (<:tr
       (<:td :class "title"
	     (<:p "Status"))
       (<:td :class "desc"
	     (<:p :id "ticket-state" (<:ah (state-string (state ticket))))
	     (<:p :id "ticket-state-e" :style "display:none;"
		  (render (state-field ticket-form)))))
      (<:tr
       (<:td :class "title"
	     (<:p "Categories"))
       (<:td :class "desc"
	     (<:p :id "ticket-category"
		  (<:ah (let ((tags (tags-to-string (tags ticket))))
			  (if (equal tags "")
			      (<:i "(Uncategorized)")
			      tags))))
	     (<:p :id "ticket-category-e" :style "display:none;"
		  (render (tags-field ticket-form)))))
      (<:tr
       (<:td :class "title"
	     (<:p "Assigned to"))
       (<:td :class "desc"
	     (<:p :id "ticket-assignment"
		  (<:ah (if (assigned-user ticket)
			    (username (assigned-user ticket))
			    (<:i "(Unassigned)"))))
	     (<:p :id "ticket-assignment-e" :style "display:none;"
		  (render (assigned-user-field ticket-form)))))
      (<:tr
       (<:td :class "title"
	     (<:p "Priority"))
       (<:td :class "desc"
	     (<:p :id "ticket-priority"
		  (<:ah (priority-string (priority ticket))))
	     (<:p :id "ticket-priority-e" :style "display:none;"
		  (render (priority-field ticket-form)))))
      (<:tr
       (<:td :class "title"
	     (<:p "Due Date"))
       (<:td :class "desc"
	     (<:p :id "ticket-due"
		  (<:ah (if (due-date ticket)
			    (print-date (due-date ticket) :long-day) 
			    (<:i "(None)"))))
	     (<:p :id "ticket-due-e" :style "display:none;"
		  (render (due-date-field ticket-form)))))
      (<:tr
       (<:td :class "title"
	     (<:p "Response Email"))
       (<:td :class "desc"
	     (<:p :id "ticket-response" 
		  (<:ah (or (response-email ticket) (<:i "(None)"))))
	     (<:p :id "ticket-response-e" :style "display:none;"
		  (render (response-email-field ticket-form)))))
      (<:tr
       (<:td :class "title"
	     (<:p "Description"))
       (<:td :class "desc"
             (let ((desc-lines (split-sequence:split-sequence #\Newline (body ticket))))
               (<:div :id "ticket-description"
                      (dolist (line desc-lines)
                        (<:p (<:as-html line))))
               (when (> (length desc-lines) 5)
                 (setf (ucw::rows (description-field ticket-form)) (length desc-lines)))
               (<:p :id "ticket-description-e" :style "display:none;"
                    (render (description-field ticket-form))))))
      (let ((edit-js (format nil "javascript: ~a"
			     (js:js-to-string
			      `(progn
				 (.hide -element "ticket-state")
				 (.-appear -effect "ticket-state-e")
				 (.hide -element "ticket-category")
				 (.-appear -effect "ticket-category-e")
				 (.hide -element "ticket-assignment")
				 (.-appear -effect "ticket-assignment-e")
				 (.hide -element "ticket-priority")
				 (.-appear -effect "ticket-priority-e")
				 (.hide -element "ticket-due")
				 (.-appear -effect "ticket-due-e")
				 (.hide -element "ticket-response")
				 (.-appear -effect "ticket-response-e")
				 (.hide -element "ticket-description")
				 (.-appear -effect "ticket-description-e")
				 (.hide -element "edit-button")
				 (.-appear -effect "save-button")
				 (.-appear -effect "cancel-button")
				 (return false)))))
	    (unedit-js (format nil "javascript: ~a"
			       (js:js-to-string
				`(progn
				   (.hide -element "ticket-state-e")
				   (.-appear -effect "ticket-state")
				   (.hide -element "ticket-category-e")
				   (.-appear -effect "ticket-category")
				   (.hide -element "ticket-assignment-e")
				   (.-appear -effect "ticket-assignment")
				   (.hide -element "ticket-priority-e")
				   (.-appear -effect "ticket-priority")
				   (.hide -element "ticket-due-e")
				   (.-appear -effect "ticket-due")
				   (.hide -element "ticket-response-e")
				   (.-appear -effect "ticket-response")
				   (.hide -element "ticket-description-e")
				   (.-appear -effect "ticket-description")
				   (.hide -element "save-button")
				   (.hide -element "cancel-button")
				   (.-appear -effect "edit-button")
				   (return false))))))
	(<:tr
	 :class "action-bar"
	 (<:td :colspan 2 
	       (<:ul 
		:class "filter-list"
		(<:li :class "first-item" :id "edit-button"
		      (<ucw:a :action (edit-ticket page ticket)
			      :id "edit-button"
			      :onclick edit-js
			      "edit"))
		(<:li 
		 :class "first-item" :id "save-button" :style "display: none;"
		 (<:a
		  :href "#"
		  :onclick "javascript:$('edit-ticket-details').submit();"
		  "save"))
		(<:li :id "cancel-button" :style "display: none;"
		      (<:a :href "#"
			   :onclick unedit-js
			   "cancel"))
		(<:li (grey-box "Reply" "reply" (add-reply-to-ticket page ticket) 270 510))
		(<:li (grey-box "Comment" "comment" (add-comment-to-ticket page ticket) 270 510))
	        (unless (equal (state ticket) +ticket-status-closed+)
	          (<:li (grey-box "Close Ticket" "close" (close-ticket page ticket) 270 510)))
		(<:li (<ucw:a :action (delete-ticket page ticket) "delete"))))))))))


(defaction delete-ticket ((page ticket-tabbed-view) ticket)
  (delete-ticket% ticket)
  (call-component nil (make-instance 'tickets-page :user (user page))))

;; ticket history - all of the individual events that have occurred for this ticket
(defun render-history-item-row (badge title timestamp content &key (newline nil) (comment nil))
  (<:tr
   (<:td :rowspan 2 :align "center" :class "badge"
	 (<:img :src (format nil "/images/cond-~a.gif" badge)
		:width "24px" :height "24px"))
   (<:td :class "name"
	 (<:p (<:as-html title) (<:span (<:as-is " at ") (<:as-html timestamp)))))
  (<:tr
   (<:td :class "description" 
	 (if newline
	     (dolist (line (split-sequence:split-sequence #\Newline content))
	       (<:p (<:as-html line)))
	     (<:p (<:as-html content)))
	 (when comment
	   (<:p (<:b "Comment: ") (<:as-html comment))))))


(defgeneric render-history-item (item ticket user)
  (:method ((response db::ticket-response) ticket user)
    (render-history-item-row (if (db::user-id response) "Respond" "Submit")
			     (sender response) 
			     (adjusted-timestamp response user) 
			     (body response)
			     :newline t))
  (:method ((comment db::ticket-comment) ticket user)
    (let ((poster (user comment)))
      (render-history-item-row "Note" 
			       (if (and (name poster) 
					(not (equal (name poster) ""))) 
				   (name poster) 
				   (username poster)) 
			       (adjusted-timestamp comment user)
			       (body comment))))
  (:method ((change db::ticket-change) ticket user)
    (let ((changer (user change))
	  (note (note change)))
      (render-history-item-row "Change" 
			       (if (and (name changer) 
					(not (equal (name changer) ""))) 
				   (name changer) 
				   (username changer)) 
			       (adjusted-timestamp change user)
			       (description change)
			       :comment (if (and note (not (equal note ""))) 
					    note
					    nil)))))

(defmethod render-ticket-history-list ((page ticket-tabbed-view))
  (let ((ticket (ticket page))
	(user (user page)))
    (<:h2 "History")
    (<:table :cellspacing 3
     :class "item-list condensed" :id "tickets"
     (let* ((responses (select 'db::ticket-response :flatp t :where [= [ticket-id] (id ticket)]))
	    (comments (select 'db::ticket-comment :flatp t :where [= [ticket-id] (id ticket)]))
	    (changes (select 'db::ticket-change :flatp t :where [= [ticket-id] (id ticket)]))
	    (sorted-list (sort (append responses comments changes) #'timestamp<)))
       (if (zerop (length sorted-list))
	   (<:as-html "No history for this ticket")
	   (dolist (history-item sorted-list)
	     (render-history-item history-item ticket user)))))))

(defmethod render-ticket-computer-list ((page ticket-tabbed-view))
  (let ((ticket (ticket page))
	(user (user page)))
    (<:h2 "Computers ")
    (<:ul :class "filter-list"
	  (<:li :class "select-all first-item"
		(grey-box "Add/Remove Computers" "Add/Remove"
			  (open-computer-ticket-dialog page ticket) 320 500)))
    (let ((computers (select 'computer :flatp t :order-by [slot-value 'computer 'alias]
			     :where [and
			     [= [slot-value 'computer 'company-id] (company-id user)]
			     [= [slot-value 'ticket-computer 'ticket-id] (id ticket)]
			     [= [slot-value 'ticket-computer 'computer-id] [slot-value 'computer 'id]]])))
      (unless computers
	(<:p "There are no computers currently associated with this ticket"))
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
		 (<:ah (name computer))))))))))


#.(clsql:restore-sql-reader-syntax-state)
