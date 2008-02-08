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


(defaction goto-tickets-page ((page paragent-component) &key ticket-id)
  (let ((callee (make-instance 'tickets-page :user (user page))))
    (navigate-to-item (ticket-list callee) ticket-id)
    (call-component nil callee)))



;;; ticket list component on the page

(defcomponent ticket-list (filtered-item-list)
  ((filter :accessor filter
           :initarg :filter
           :initform nil
           :documentation "Holds the open/closed filter")
   (assigned-filter :accessor assigned-filter
                    :initform nil
                    :documentation "Holds the filter for who the tickets are assigned to")
   (use-saved-filters :accessor use-saved-filters
                      :initarg :use-saved-filters
                      :initform nil
                      :type boolean
                      :documentation "Tells it to set up filters based on the user's cookies")
   (tag-filter :accessor tag-filter
               :initform nil
               :documentation "Holds the filter for tags")
   (expanded-id :accessor expanded-id
                :initarg :expanded-id
                :initform nil
                :type integer)
   (show-checkboxes :accessor show-checkboxes
                    :initarg :show-checkboxes
                    :initform nil
                    :type boolean))
  (:default-initargs
    :div-id "tickets"
    :results-per-page 30
    :use-optimized t
    :cellspacing 0
    :default-view-classes (list [tickets])))

(defcomponent inline-ticket-list (ticket-list)
  ())

;;; The tickets page itself

(defcomponent tickets-page (paragent-window-component)
  ((ticket-list :accessor ticket-list
                :type ticket-list)
   (selected-id :accessor selected-id
		:initarg :selected-id
		:initform ""))
  (:default-initargs
    :title "Tickets"))

(defmethod initialize-instance :after ((page tickets-page) &key ticket-id)
  (with-db 
    (setf (ticket-list page)
          (make-instance 'ticket-list :user (user page) :expanded-id ticket-id 
                         :show-checkboxes t :use-saved-filters t))))

(defmethod body-id ((component tickets-page))
  "tickets")

(defmethod render-tasks ((page tickets-page))
  (<:h1 "Tickets")
  (<:ul
    (<:li 
     (grey-box "New Ticket" "New Ticket..."
                    (new-ticket page) 439 530))
    (<:li
     (<:a :href (format nil "/submit-ticket.ucw?company=~a" (name (company (user page)))) "External Ticket Form..."))
    (when (>=admin (user page))
      (<:li (grey-box "Watch this email address..." "Add email account to monitor..."
		      (goto-ticket-email-dialog page) 250 360))))
  
  (<:h1 "Recent Tickets")
  (<:ul
   (<:li (<:a :href *url-tickets* "All Tickets"))
   (dolist (recent-ticket (sort (recent-tickets (user page)) (lambda (x y) (> (id x) (id y)))))
     (let ((ticket (ticket recent-ticket)))
       (when ticket
         (<:li (<:a :href (ticket-link ticket)
                    (<:ah (ticket-id ticket) " - " (subject ticket)))))))))

(defrender ((page tickets-page))
  (init-grey-box)
  (let ((user (user page))
        (input-id1 (unique-id "selected-tickets")))
    (<ucw:script
      `(progn
         (defvar input-id2 ,input-id1)))
    (<:script :src "checkbox.js" :type "text/javascript")
    (show-tip user
              "Paragent also provides you with a trouble ticket tracking system. With our integrated solution, you can easily link tickets to the computers on which they occurred.")
    (setf (selected-id page) input-id1)
    (render (ticket-list page))))

(defmethod render-batch-action-toolbar ((page tickets-page))
  (let* ((ticket-ids "")
	 (input-id (selected-id page)))
    (<:div
      :class "batch-action-toolbar"
      (<ucw:form
        (<:div
          (<ucw:input :type "hidden" :accessor ticket-ids :id input-id)
	  (left-right-align
	    (progn
	      (<ucw:input :action (do-batch-close-tickets page ticket-ids) :type "image"
	                  :src "images/closeticketbtn.gif" :value "Close")
	      (<ucw:input :action (do-batch-reopen-tickets page ticket-ids) :type "image"
	                  :src "images/reopenbtn.gif" :value "Reopen ticket"))
	    (<ucw:input :action (do-batch-delete-tickets page ticket-ids) :type "image"
	                :onclick "return confirm('Delete these tickets?');"
	                :src "images/deletebtn.gif")))))))

(defgeneric priority-string (ticket)
  (:documentation "Returns a string denoting the priority of the ticket"))

(defmethod priority-string ((ticket ticket))
  (priority-string (priority ticket)))

(defmethod priority-string ((priority integer))
  (case priority
        (0 "Note")
        (1 "Low")
        (2 "Medium")
        (3 "High")
        (4 "Critical")))

(defgeneric state-string (ticket)
  (:documentation "Returns a string denoting the state of the ticket"))

(defmethod state-string ((ticket ticket))
  (state-string (state ticket)))

(defmethod state-string ((state integer))
  (case state
        (0 "Closed")
        (1 "Open")
        (2 "On Hold")))


(defaction delete-ticket ((page paragent-component) ticket)
  (delete-ticket% ticket))

(defun delete-ticket% (ticket)
  (with-db
   (delete-records :from [recent-tickets]
                   :where [= [ticket-id] (id ticket)])
   (delete-instance-records ticket)))



(defaction do-batch-delete-tickets ((page paragent-component) ticket-ids)
  (do-batch-delete-tickets% page (parse-ids ticket-ids)))

(defmethod do-batch-delete-tickets% ((page paragent-component) ticket-ids)
  (when ticket-ids
    (with-db
      (let ((ticket-ids (select [slot-value 'ticket 'id] :from [tickets] :flatp t
                                :where [and [in [slot-value 'ticket 'id] ticket-ids]
                                [= [slot-value 'ticket 'company-id] (company-id (user page))]])))
        (when ticket-ids
          (flet ((delete-ticket-prop (table object)
                   (delete-records :from table
                                   :where [in [slot-value object 'id] ticket-ids])))
            (delete-ticket-prop [ticket-changes] 'ticket-change)
            (delete-ticket-prop [ticket-comments] 'ticket-comment)
            (delete-ticket-prop [ticket-computers] 'ticket-computer)
            (delete-ticket-prop [ticket-emails] 'ticket-email)
            (delete-ticket-prop [ticket-responses] 'ticket-response)
            (delete-ticket-prop [ticket-tags] 'ticket-tag))
          (delete-records :from [tickets] :where [and
                          [= [slot-value 'ticket 'company-id] (company-id (user page))]
                          [in [slot-value 'ticket 'id] ticket-ids]]))))))

(defaction do-batch-close-tickets ((page paragent-component) ticket-ids)
  (do-batch-close-tickets% page (parse-ids ticket-ids)))

(defmethod do-batch-close-tickets% ((page paragent-component) ticket-ids)
  (when ticket-ids
    (with-db
      (update-records [tickets]
                      :attributes '([state])
                      :values (list +ticket-status-closed+)
                      :where [and
                      [= [slot-value 'ticket 'company-id] (company-id (user page))]
                      [in [slot-value 'ticket 'id] ticket-ids]]))))

(defaction do-batch-reopen-tickets ((page paragent-component) ticket-ids)
  (do-batch-reopen-tickets% page (parse-ids ticket-ids)))

(defmethod do-batch-reopen-tickets% ((page paragent-component) ticket-ids)
  (when ticket-ids
    (with-db
      (update-records [tickets]
                      :attributes '([state])
                      :values (list +ticket-status-open+)
                      :where [and
                      [= [slot-value 'ticket 'company-id] (company-id (user page))]
                      [in [slot-value 'ticket 'id] ticket-ids]])))) 

(defmethod initialize-instance :after ((page ticket-list) &key)
  (when (expanded-id page)
    (navigate-to-item page (expanded-id page)))
  (when (use-saved-filters page)
    (let  ((ticket-open-filter (get-cookie "ticket-open-filter"))
           (ticket-person-filter (get-cookie "ticket-person-filter")))
      (when (not-blank ticket-open-filter)
        (cond
          ((equal ticket-open-filter "All") 
           (setf (filter page) [= 1 1])
           (set-filter page :open-closed
                       [= 1 1]
                       [tickets]))
          (t
           (let ((state (parse-integer ticket-open-filter :junk-allowed t)))
             (setf (filter page) [= [slot-value 'ticket 'state] state])
             (set-filter page :open-closed
                         [= [slot-value 'ticket 'state] state]
                         [tickets])))))
      (when (not-blank ticket-person-filter)
        (cond
          ((equal ticket-person-filter "Nobody")
           (setf (assigned-filter page) [null [slot-value 'ticket 'assigned-user-id]])
           (set-filter page :assigned 
                       [null [slot-value 'ticket 'assigned-user-id]]
                       [tickets]))
          ((equal ticket-person-filter "Anybody")
           (setf (assigned-filter page) [= 1 1])
           (set-filter page :assigned 
                       [= 1 1]
                       [tickets]))
          (t
           (let ((id (parse-integer ticket-person-filter :junk-allowed t)))
             (setf (assigned-filter page) [= [slot-value 'ticket 'assigned-user-id] id])
             (set-filter page :assigned 
                         [= [slot-value 'ticket 'assigned-user-id] id]
                         [tickets])))))
      )))


(defaction do-filter ((page ticket-list) filter)
  (set-filter page :open-closed filter [tickets])
  (setf (filter page) filter)
  (let ((val (cond
               ((sql-equal filter [= 1 1]) "All")
               (t (second (slot-value filter 'clsql-sys::sub-expressions))))))
    (set-cookie "ticket-open-filter" (format nil "~a" val))))

(defaction do-assigned-filter ((page ticket-list) filter)
  (set-filter page :assigned filter [tickets])
  (setf (assigned-filter page) filter)
  (let ((val (cond
               ((sql-equal [null [slot-value 'ticket 'assigned-user-id]] filter) 
                "Nobody")
               ((sql-equal [= 1 1] filter) "Anybody")
               (t (second (slot-value filter 'clsql-sys::sub-expressions))))))
    (set-cookie "ticket-person-filter" (format nil "~a" val))))


(defmethod navigate-to-item ((page ticket-list) (ticket-id integer))
  (setf (expanded-id page) ticket-id)
  (with-db
    (let ((index (car
                   (select [count [distinct [slot-value 'ticket 'id]]]
                           :from (get-filter-tables page)
                           :distinct t :flatp t
                           :where [and
                           [= [slot-value 'ticket 'company-id] (company-id (user page))]
                           [< [slot-value 'ticket 'id] ticket-id]
                           (get-filter-sql page)]))))
      (setf (from page) (floor index (results-per-page page))))))
      

(defgeneric get-categories (company)
  (:documentation "Returns all ticket categories defined for the company"))

(defmethod get-categories ((company company))
  "Returns all the categories that are defined within a company"
  (get-categories (id company)))

(defmethod get-categories ((company-id integer))
  "Returns all the categories that are defined for a company with the given id"
  (select [slot-value 'ticket-tag 'name] :refresh t :flatp t :distinct t
          :from (list [ticket-tags] [tickets])
          :order-by [slot-value 'ticket-tag 'name]
          :where [and [= [slot-value 'ticket-tag 'ticket-id] [slot-value 'ticket 'id]]
          [= [slot-value 'ticket 'company-id] company-id]]))


(defmethod render-title ((page ticket-list))
  (<:div
    :class "box-title" :id "ticket-title"
    (<:h2 "Tickets")
    (<:ul :class "filter-list"
	  (<:li :class "first-item"
		(let ((filter (filter page)))
		  (<ucw:form
		   :action (do-filter page filter)
		   (<drop-down-field
		    :accessor filter
		    :onchange "this.form.submit()"
		    :test #'sql-equal
		    :data-set `(("All" . ,[= 1 1])
				("Open" . ,[= [slot-value 'ticket 'state] 1])
				("Closed" . ,[= [slot-value 'ticket 'state] 0])
				("On Hold" . ,[= [slot-value 'ticket 'state] 2])))))
		(let ((filter (assigned-filter page)))
		  (<ucw:form
		   :action (do-assigned-filter page filter)
		   (<:ah " Tickets assigned to ")
		   (<drop-down-field
		    :accessor filter
		    :onchange "this.form.submit()"
		    :test #'sql-equal
		    :data-set `(("Anybody" . ,[= 1 1])
				("You" . ,[= [slot-value 'ticket 'assigned-user-id] (id (user page))])
				,@(mapcar
				   (lambda (user)
				     (cons (username user) 
					   [= [slot-value 'ticket 'assigned-user-id] (id user)]))
				   (select 'user :flatp t :order-by [username]
					   :where [and [= [company-id] (company-id (user page))]
					   [not [= [id] (id (user page))]]
					   ]))
				("Nobody" . ,[null [slot-value 'ticket 'assigned-user-id]]))))))
	  (<:li :class "category-filter first-item"
	   (let ((filter (tag-filter page)))
	     (<:ah "Category: ")
	     (<ucw:form
	      :action (do-tag-filter page filter)
	      (<drop-down-field
	       :accessor filter
	       :onchange "this.form.submit()"
	       :test #'sql-equal
	       :data-set `(("All" . ,[= 1 1])
			   ,@(mapcar
                              (lambda (category)
                                (cons
				 category
				 [exists [select [slot-value 'ticket-tag 'id] :from [ticket-tags]
				 :where [and [like [slot-value 'ticket-tag 'name] category]
				 [= [slot-value 'ticket-tag 'ticket-id] [slot-value 'ticket 'id]]]]]))
                              (get-categories (company-id (user page))))))))))))

(defmethod render-title ((page inline-ticket-list))
  (<:h2 "Tickets")
  (<:ul :class "filter-list"
	(<:li :class "first-item"
	      (let ((filter (filter page)))
		(<ucw:form
		 :action (do-filter page filter)
		 (<drop-down-field
		  :accessor filter
		  :onchange "this.form.submit()"
		  :test #'sql-equal
		  :data-set `(("All" . ,[= 1 1])
			      ("Open" . ,[= [slot-value 'ticket 'state] 1])
			      ("Closed" . ,[= [slot-value 'ticket 'state] 0])
			      ("On Hold" . ,[= [slot-value 'ticket 'state] 2])))))
	      (let ((filter (assigned-filter page)))
		(<ucw:form
		 :action (do-assigned-filter page filter)
		 (<:ah " Tickets assigned to ")
		 (<drop-down-field
		  :accessor filter
		  :onchange "this.form.submit()"
		  :test #'sql-equal
		  :data-set `(("Anybody" . ,[= 1 1])
			      ("You" . ,[= [slot-value 'ticket 'assigned-user-id] (id (user page))])
			      ,@(mapcar
				 (lambda (user)
				   (cons (username user) 
					 [= [slot-value 'ticket 'assigned-user-id] (id user)]))
				 (select 'user :flatp t :order-by [username]
					 :where [and [= [company-id] (company-id (user page))]
					 [not [= [id] (id (user page))]]
					 ]))
			      ("Nobody" . ,[null [slot-value 'ticket 'assigned-user-id]]))))))
	(<:li :class "category-filter first-item"
	      (let ((filter (tag-filter page)))
		(<:ah "Category: ")
		(<ucw:form
		 :action (do-tag-filter page filter)
		 (<drop-down-field
		  :accessor filter
		  :onchange "this.form.submit()"
		  :test #'sql-equal
		  :data-set `(("All" . ,[= 1 1])
			      ,@(mapcar
				 (lambda (category)
				   (cons
				    category
				    [exists [select [slot-value 'ticket-tag 'id] :from [ticket-tags]
				    :where [and [like [slot-value 'ticket-tag 'name] category]
				    [= [slot-value 'ticket-tag 'ticket-id] [slot-value 'ticket 'id]]]]]))
				 (get-categories (company-id (user page)))))))))))

(defmethod render ((page inline-ticket-list))
  ;; Javascript
  (<:script :type "text/javascript"
            (<:as-is
              (render-js page)))
  ;; Html
  (render-title page)
  (render-html page))



(defaction do-tag-filter ((page ticket-list) filter)
  (set-filter page :category filter [ticket-tags])
  (setf (tag-filter page) filter))

(defmethod optimized-list-count ((page ticket-list))
  (car
    (select [count [distinct [slot-value 'ticket 'id]]]
            :from (get-filter-tables page)
            :distinct t :flatp t
            :where [and
            [= [slot-value 'ticket 'company-id] (company-id (user page))]
            (get-filter-sql page)])))
    

(defmethod optimized-list-getter ((page ticket-list) start len)
  (select 'ticket :flatp t :distinct t
          :where [and
          [= [slot-value 'ticket 'company-id] (company-id (user page))]
          (get-filter-sql page)]
          :order-by (list (list [slot-value 'ticket 'id] :desc))
          :offset start :limit len ))

(defmethod render-item ((page ticket-list) ticket)
  (declare (type ticket ticket))
  (let ((class (if (equal (state ticket) +ticket-status-closed+)
		   "Closed"
		   (db::priority-name ticket)))
        (odd-row (if (odd-row page) "odd-row" "even-row"))
	(user (user page)))
    (if (not (condensed page))
	(progn
	  (<:tr
	   :class odd-row
	   (<:td :rowspan 3 :align "center" :class (format nil "badge ~a" class)
		 (<:span :class "state" (<:as-html (state-string ticket))))
	   (<:td :class "name" 
	         (<:span (<:a :href (ticket-link ticket)
	                      (<:as-html (ticket-id ticket)) 
	                      (<:as-is " &ndash; ")
			      (<:ah (subject ticket))
			      (when (timestamp ticket)
				(<:span
				 (<:as-is " at ")
				 (<:ah (db::adjusted-timestamp ticket user))))))))
	  (<:tr
	   :class odd-row
	   (<:td :class "summary" (<:p
				   (<:span "Due Date: ")
				   (if (due-date ticket)
				       (<:ah (<:ah (print-date (due-date ticket) :long-day)))
				       (<:i"(None)"))
				   (<:span " |  Assigned to: ")
				   (<:ah (if (assigned-user ticket)
					     (username (assigned-user ticket))
					     (<:i "(Unassigned)"))))))
	(<:tr
	 :class (format nil "action-bar ~a" odd-row)
	 (<:td (<:ul
		:class "filter-list"
		(<:li :class "first-item" (<:a :href (ticket-link ticket) "details"))
		(<:li (grey-box "Reply" "reply" (add-reply-to-ticket page ticket) 270 510))
		(<:li (grey-box "Comment" "comment" (add-comment-to-ticket page ticket) 270 510))
	        (unless (equal (state ticket) +ticket-status-closed+)
	          (<:li (grey-box "Close Ticket" "close" (close-ticket page ticket) 270 510)))
		(<:li (<ucw:a :action (delete-ticket page ticket) "delete"))))))
	(progn
	  (<:tr
	   :class odd-row
	   (<:td :rowspan 2 :align "center" :class "badge"
		 (<:img :src (format nil "/images/cond-~a.gif" class)
			:width "24px" :height "24px"))
	   (<:td :class "name" 
		 (<:span (<:as-html (ticket-id ticket)) 
			 (<:as-is " &ndash; ")
			 (<:a :href (ticket-link ticket)
			      (<:ah (subject ticket))))))
	  (<:tr
	   :class odd-row
	   (<:td :class "summary"
		 (<:p
		  (<:span "Due Date: ")
		  (if (due-date ticket)
		      (<:ah (<:ah (print-date (due-date ticket) :long-day)))
		      (<:i"(None)"))
		  (<:span " |  Assigned to: ")
		  (<:ah (if (assigned-user ticket)
			    (username (assigned-user ticket))
			    (<:i "(Unassigned)"))))))))
    (<:tr (<:td (<:br)) (<:td))))


(defmethod render-none ((page ticket-list))
  (<:tr
    (<:td :colspan 0 :align "center"
          (<:h3 "No tickets found")))
  (<:tr :class "blank" (<:td :colspan 0 (<:p (<:ai "&nbsp;")))))


;;; Specialized computer-list

(defcomponent ticket-computer-list (computer-list)
  ((parent :accessor parent
           :initarg :parent)
   (ticket :accessor ticket
           :type ticket
           :initarg :ticket)))

(defmethod initialize-instance :after ((page ticket-computer-list) &key)
  (set-filter page :tickets
              [and
              [= [slot-value 'ticket-computer 'ticket-id] (id (ticket page))]
              [= [slot-value 'ticket-computer 'computer-id] [slot-value 'computer 'id]]]
              [ticket-computers]))

(defmethod render-none ((page ticket-computer-list))
  (<:p "There are no tickets associated with this computer"))


(defaction reopen-ticket ((page ticket-list) ticket)
  (set-ticket-status (user page) ticket 1)
  (setf (expanded-id page) (id ticket)))

(defun status-change-description (old new)
  (format nil
          (case new
                (#.+ticket-status-closed+ "Ticket was closed (had been ~a).")
                (#.+ticket-status-open+ "Ticket was reopened (had been ~a).")
                (#.+ticket-status-hold+ "Ticket was put on hold (had been ~a).")
                (otherwise "Ticket was put on hold (had been ~a)."))
          (state-string old)))

(defun set-ticket-status (user ticket status)
  (declare (type user user))
  (declare (type ticket ticket))
  
  (with-db
    (update-records-from-instance
      (make-instance 'ticket-change :ticket-id (id ticket)
                     :user-id (id user)
                     :description (status-change-description (state ticket) status)))
    (setf (state ticket) status)
    (update-records-from-instance ticket)))




#.(clsql:restore-sql-reader-syntax-state)
