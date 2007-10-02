;;;; Created on 2006-05-31 10:27:06

(in-package :nexus)

#.(clsql:locally-enable-sql-reader-syntax)

(defentry-point "computer-details.ucw" (:application *my-app*)
  (id)
  (let ((user (get-user)))
    (if user
        (let ((computer (computer-for-name id (company-id user))))
          (if computer
              (call 'computer-page :user user :computer computer :title id)
              (call 'redirect-component :target "computers.ucw")))
        (call 'redirect-component :target "login.ucw"))))

(defgeneric computer-link (computer)
  (:documentation "Returns the a link to the computer"))

(defmethod computer-link ((computer computer))
  (format nil "computer-details.ucw?id=~a" (name computer)))

(defmethod computer-link ((name string))
  (format nil "computer-details.ucw?id=~a" name))

(defcomponent computers-page (paragent-window-component)
  ((computer-list :accessor computer-list
                  :initarg :computer-list
		  :type computer-list)
   (error-message :accessor error-message
		  :initarg :error-message
		  :initform nil))
  (:default-initargs
   :title "Computers"))

(defaction goto-computers-page ((page paragent-component) user &key computer (error-message nil))
  (if computer
      (call-component nil (make-instance 'computer-page 
					 :user user 
					 :computer computer
					 :error-message error-message
					 :title (name computer)))
      (call-component nil (make-instance 'computers-page 
					 :user user
					 :error-message error-message))))

(defaction goto-online-computers ((page paragent-component))
  (let* ((computer-page (make-instance 'computers-page :user (user page)))
         (computer-list (computer-list computer-page)))
    (set-filter computer-list :online [not [= [slot-value 'computer 'online] t]] [computers])
    (setf (title computer-list) "Online Computers")
    (call-component nil computer-page)))

(defaction goto-offline-computers ((page paragent-component))
  (let* ((computer-page (make-instance 'computers-page :user (user page)))
         (computer-list (computer-list computer-page)))
    (set-filter computer-list :online [= [slot-value 'computer 'online] t] [computers])
    (setf (title computer-list) "Offline Computers")
    (call-component nil computer-page)))


(defmethod body-id ((component computers-page))
  "computers")

(defmethod initialize-instance :after ((page computers-page) &key (title "Computers"))
  (setf (computer-list page)
        (make-instance 'computer-list 
		       :user (user page)
                       :show-checkboxes t
		       :title title
                       :results-per-page 30 
                       :place (make-place (computer-list page)))))

(defrender ((page computers-page))
  (let ((input-id1 (unique-id "selected-comps")))
    (<:script :src "actb/actb.js" :type "text/javascript")
    (<:script :src "actb/common.js" :type "text/javascript")
    (init-grey-box)
    (<ucw:script
      `(progn
         (defvar input-id1 ,input-id1)))
    (<:script :src "checkbox.js" :type "text/javascript")
    (show-tip (user page)
              "This page lists computers on your network. You can filter the computers using the \"Filter\" option at the top. Administrators can even shutdown, restart or remote to a computer from here.")
    (setf (selected-id (computer-list page)) input-id1)
    (render (computer-list page)))
  (when (error-message page)
    (<:script :type "text/javascript"
	      (<:ai (format nil "alert(\"~a\")" (error-message page))))))

;;; Computer list

(defconstant +max-filters+ 3)

(defcomponent filter-drop-down (drop-down-field)
  ((label :accessor label
	  :initarg :label
	  :initform nil)))

(defmethod initialize-instance :after ((field filter-drop-down) &key value)
  (if value
      (setf (value field) value)
      (when (label field)
	(setf (slot-value field 'client-value) (label field)))))


(defcomponent computer-list (filtered-item-list)
  ((show-checkboxes :accessor show-checkboxes
                    :initarg :show-checkboxes
		    :type boolean
                    :initform nil)
   (tag-filter :accessor tag-filter
               :initarg :tag-filter
               :initform "")
   (tag-filters :accessor tag-filters)
   (title :accessor title
          :initarg :title
	  :type string
          :initform "Computers")
   (selected-id :accessor selected-id
		:initarg :selected-id
		:initform "")
   (operating-systems :accessor operating-systems
                      :type hash-table
                      :documentation "We query all the computer operating systems at once to reduce calls to the db"))
  (:default-initargs
    :use-optimized t
    :div-id "comppick"
    :cellspacing 0
    :default-view-classes (list [computers])))

(defmethod initialize-instance :after ((page computer-list) &key)
  (with-db
    (setf (tag-filters page)
          (loop for i from 1 to +max-filters+ collecting
                nil))))

(defun parse-ids (computer-ids-string)
  (mapcar (lambda (num) (parse-integer num :junk-allowed t))
          (split-sequence:split-sequence #\Space computer-ids-string :remove-empty-subseqs t)))

(defun computers-for-ids (computer-ids user)
  (with-db
    (let* ((selected (parse-ids computer-ids))
           (computers (if selected (select 'computer 
					   :flatp t
                                           :where [and [= [company-id] (company-id user)]
						       [in [id] selected]])
                          nil)))
      computers)))


(defgeneric render-batch-action-toolbar (page)
  (:documentation "Renders the toolbar allowing you to act on multiple computers at once."))

(defmethod render-batch-action-toolbar ((page computer-list))
  (let* ((computer-ids "")
         (tag "")
         (user (user page))
	 (input-id (selected-id page))
         (has-note-permission (has-note-permission user)))
    (<:div 
     :id "batch-action-bar"
     :style "display: none;"
     (<ucw:form
      :action (do-batch-tag page computer-ids tag)
      (<:ul
       :class "filter-list"
       :id "ab-contents"
       :style "display: none;"
       (when has-note-permission
	 (<:li 
	  :class "first-item"
	  (<ucw:input :type "hidden" :accessor computer-ids :id input-id)
	  (let ((tag-id (unique-id "batch-tags")))
	    (<ucw:text
	     :accessor tag
	     :class "tag-field"
	     :id tag-id
	     :onfocus (format nil "javascript:~a"
			      (js:js-to-string
			       `(actb ($ ,tag-id)
				      (list ,@(get-tags (company-id user)))))))
	    (<ucw:input :action (do-batch-tag page computer-ids tag)
			:type "image"
			:class "button"
			:src "images/batagbtn.gif"))))
       (<:li
	(<ucw:input :action (edit-ticket page (make-default-ticket user)
					 :computer-ids (parse-ids computer-ids))
		    :type "image"
		    :class "button"
		    :src "images/banewticketbtn.gif"))
       (when has-note-permission
	 (let ((warranty-field (make-instance 'calendar-date-field)))
	   (<:li
	    (render warranty-field)
	    (<ucw:input :action (do-batch-warranty page computer-ids warranty-field)
			:class "button"
			:type "image"
			:src "images/basetwarrantybtn.gif"))))
       (when (has-shutdown-permission user)
	 (<:li (<ucw:input :action (do-batch-restart page computer-ids)
			   :onclick "return confirm('Reboot these computers?');"
			   :type "image"
			   :class "button"
			   :src "images/barestartbtn.gif"))
	 (<:li (<ucw:input :action (do-batch-shutdown page computer-ids)
			   :onclick "return confirm('Shutdown these computers?');"
			   :type "image"
			   :class "button"
			   :src "images/bashutdownbtn.gif")))
       (when has-note-permission
	 (<:li 
	  (<ucw:input :action (do-batch-delete page computer-ids)
		      :onclick "return confirm('Delete these computers?');"
		      :type "image"
		      :class "button"
		      :src "images/badeletebtn.gif"))))))))

(defaction do-batch-warranty ((page paragent-component) computer-ids date-field)
  (do-batch-warranty% page computer-ids date-field))

(defun do-batch-warranty% (page computer-ids date-field)
  (with-db
    (let* ((user (user page))
           (computers (computers-for-ids computer-ids user))
           (warranty (value-as-date date-field)))
      (when computers
        (dolist (computer computers)
          (when (has-note-permission user computer)
            (setf (warranty computer) warranty)
            (update-records-from-instance computer)))))))
    

(defaction do-batch-delete ((page computer-list) computer-ids)
  (do-batch-delete% page computer-ids))

(defun do-batch-delete% (page computer-ids)
  (with-db
    (let* ((computers (computers-for-ids computer-ids (user page))))
      (remove-computers% page
                         (loop for computer in computers
                               unless (online computer)
                               collecting computer)))))
      

(defaction do-batch-restart ((page computer-list) computer-ids)
  (send-batch-command page computer-ids "(restart ~A)" #'has-shutdown-permission))

(defaction do-batch-shutdown ((page computer-list) computer-ids)
  (send-batch-command page computer-ids "(shutdown ~A)" #'has-shutdown-permission))

(defgeneric send-batch-command (page computer-ids command permission-checker)
  (:documentation "Sends a command to a whole bunch of computers"))

(defmethod send-batch-command ((page computer-list) computer-ids command permission-checker)
  (let ((user (user page)))
    (dolist (computer (computers-for-ids computer-ids (user page)))
      (when (funcall permission-checker user computer)
        (send-archon computer (format nil command (id computer)))))))

(defaction do-batch-tag ((page computer-list) computer-ids tag)
  (do-batch-tag% page computer-ids tag))


(defgeneric do-batch-tag% (page computer-ids tag))

(defmethod do-batch-tag% ((page computer-list) computer-ids tag)
  (with-db
    (let ((user (user page)))
      (dolist (computer (computers-for-ids computer-ids user))
        (let* ((old-tags (render-tags (tags computer)))
               (new-tags (concatenate 'string old-tags " " tag)))
          (when (has-note-permission user computer)
            (save-tags computer new-tags)))))))

(defun filter-count (filters)
  (cond ((third filters) 3)
	((second filters) 2)
	((first filters) 1)
	(t 0)))

(defmethod render-title ((page computer-list))
  (<:div 
   :class "box-title"
   (<:h2 (<:as-html (title page)))
   (render-batch-action-toolbar page)
   (<:ul 
    :class "filter-list"
    (if (car (tag-filters page))
	(<:li :class "first-item" (<ucw:a :action (remove-all-filters page) "All"))
	(<:li :class "first-item" "All"))
    (let* ((user (user page))
	   (tags-list (get-tags (company-id user)))
	   (no-filters-p (null (car (tag-filters page))))
	   (groups-list (remove-if #'default-group-p (groups (company-id user)))))
      (<ucw:form
       :action (do-filter page nil)
       (let* ((data-set (append
			 (cons (cons "Groups" nil)
			       (mapcar
				(lambda (group)
				  (let ((val (cons (list (id group) 
							 (all-computers group)) 
						   :group)))
				    (cons (name group) val)))
				groups-list))
			 (cons (cons "Tags" nil)
			       (mapcar
				(lambda (ltag)
				  (let ((val (cons ltag :tag)))
				    (cons ltag val)))
				tags-list))))
	      (filter1 (first (tag-filters page)))
	      (filter2 (second (tag-filters page)))
	      (filter3 (third (tag-filters page)))
	      (num-filters (filter-count (tag-filters page))))
	 (when (not no-filters-p)
	   (<:li :class "filter-label" "Filtering: "))
	 (<:li
	  :class (if no-filters-p "first-filter" "filter")
	  (render
	   (make-instance 
	    'filter-drop-down
	    :value  filter1
	    :label "Filter"
	    :writer (lambda (x)
		      (setf (nth 0 (tag-filters page)) x))
	    :data-set (if (> num-filters 1)
			    (cons (cons "(clear)" (cons "(clear)" :clear)) data-set)
			    data-set)
	    :onchange "if (this.value != 'Groups:' && this.value != 'Tags:') this.form.submit();"))
	  (cond
	    ((= num-filters 2)
	     (<:ah " and "))
	    ((= num-filters 3)
	     (<:ah ", ")))
	  (when (> num-filters 0)
	    (render
	     (make-instance 
	      'filter-drop-down
	      :value  filter2
	      :label (if filter2
			 "Filter"
			 '(:img "images/plus.gif"))
	      :writer (lambda (x)
			(setf (nth 1 (tag-filters page)) x))
	      :data-set (if (> num-filters 1)
			    (cons (cons "(clear)" (cons "(clear)" :clear)) data-set)
			    data-set)
	      :onchange "if (this.value != 'Groups:' && this.value != 'Tags:') this.form.submit();")))
	  (when (> num-filters 2)
	    (<:ah " and "))
	  (when (> num-filters 1)
	    (render
	     (make-instance 
	      'filter-drop-down
	      :value  filter3
	      :label (if filter3
			 "Filter"
			 '(:img "images/plus.gif"))
	      :writer (lambda (x)
			(setf (nth 2 (tag-filters page)) x))
	      :data-set (if (> num-filters 2)
			    (cons (cons "(clear)" (cons "(clear)" :clear)) data-set)
			    data-set)
	      :onchange "if (this.value != 'Groups:' && this.value != 'Tags:') this.form.submit();")))))))

   (<:li 
    :class "select-all first-item" 
    (<:a :href "#" :onclick "selectAllComputers(this);" "select all")))))

(defmethod render :after ((page computer-list))
  )

;; synchronicity finally bites us. We need the ignored parameter to match event-list's do-filter
(defgeneric do-filter% (page ignored))

(defgeneric remove-all-filters% (page))

(defaction do-filter ((page computer-list) ignored)
  (do-filter% page ignored))

(defaction remove-all-filters ((page computer-list))
  (remove-all-filters% page))

(defmethod do-filter% ((page computer-list) ignored)
  (declare (ignore ignored))
  (let ((num-filters (filter-count (tag-filters page))))
    (loop for i from 1 to +max-filters+
          for filter in (tag-filters page) do
          (let* ((filter-val filter)
                 (filter-key (format nil "tags~a" i))
                 (obj (car filter-val))
                 (key (cdr filter-val)))
            (if (<= i num-filters)
                (cond 
		  ((equal key :clear)
		   (remove-filter page filter-key)
		   (cond
		     ((= i 1)
		      (setf (tag-filters page) (list (second (tag-filters page))
						     (third (tag-filters page))
						     nil)))
		     ((= i 2)
		      (setf (tag-filters page) (list (first (tag-filters page))
						     (third (tag-filters page))
						     nil)))
		     ((= i 3)
		      (setf (tag-filters page) (list (first (tag-filters page))
						     (second (tag-filters page))
						     nil)))))
                  ((equal key :tag)
                   (set-filter page filter-key
                               [exists [select [slot-value 'computer-tag 'id] :from [computer-tag]
                               :where [and [like [slot-value 'computer-tag 'name] obj]
                               [= [slot-value 'computer-tag 'computer-id] [slot-value 'computer 'id]]]]]
                               [computer-tag]))
                  ((equal key :group)
                   (if (second obj)
                       (remove-filter page filter-key)
                       (set-filter page filter-key
                                   [and
                                   [= [slot-value 'group-computer-link 'group-id] (car obj)]
                                   [= [slot-value 'computer 'id] [slot-value 'group-computer-link 'computer-id]]]
                                   [group-computer-links])))
                  (t
                    (remove-filter page filter-key)))
                (remove-filter page filter-key)))))
    (setf (from page) 0))

(defmethod remove-all-filters% ((page computer-list))
  (remove-filter page "tags1")
  (remove-filter page "tags2")
  (remove-filter page "tags3")
  (setf (tag-filters page) (list nil nil nil)))
      
(defmethod render-js ((page computer-list))
  (js:js-to-string
    `(progn
       (defvar num-filters ,(filter-count (tag-filters page)))
       
       (defun add-filter ()
         (when (< num-filters ,+max-filters+)
           (when (= num-filters 1)
             (.-appear -effect "filter-minus"))
           (incf num-filters)
           (.-appear -effect (+ "filter" num-filters))
           (when (= num-filters 3)
             (.-fade -effect "filter-plus")))
         (setf (slot-value ($ "num-filters") 'value) num-filters)
         (return false))
       
       (defun remove-filter ()
         (when (> num-filters 1)
           (let ((doomed-filter ($ (+ "filter" num-filters))))
             (.-fade -effect doomed-filter)
             (decf num-filters)
             (when (< num-filters ,+max-filters+)
               (.-appear -effect "filter-plus"))
             (when (= num-filters 1)
               (.-fade -effect "filter-minus"))
             (setf (slot-value ($ "num-filters") 'value) num-filters)
             (if (= (slot-value doomed-filter 'selected-index) 0)
                 (return false)
                 (return t)))
           (return false)))
       
       (defun submit-maybe (filter)
         (let ((selection (slot-value (aref (slot-value filter 'options) (slot-value filter 'selected-index)) 'text)))
           (unless (or (= selection "Groups:") (= selection "Tags:"))
             (.form.submit filter)))))))
       
(defaction add-tag-filter ((page computer-list))
  (when (< (filter-count (tag-filters page)) +max-filters+)
    (do-filter page nil)))

(defaction remove-tag-filter ((page computer-list))
  (when (> (filter-count (tag-filters page)) 1)
    (do-filter page nil)))
                
(defmethod unoptimized-list-getter ((page computer-list))
  (select 'computer :refresh t :flatp t
          :order-by [slot-value 'computer 'name]
          :where [and
          [= [slot-value 'computer 'company-id] (company-id (user page))]
          (get-filter-sql page)]))

(defmethod optimized-list-count ((page computer-list))
  (caar
    (select [count [distinct [slot-value 'computer 'id]]]
            :from (get-filter-tables page)
            :distinct t
            :where [and
            [= [slot-value 'computer 'company-id] (company-id (user page))]
            (get-filter-sql page)])))
    
(defmethod optimized-list-getter ((page computer-list) start len)
  (let ((computers (select 'computer :order-by [slot-value 'computer 'alias] :flatp t :distinct t
                           :where [and
                           [= [slot-value 'computer 'company-id] (company-id (user page))]
                           (get-filter-sql page)]
                           :offset start :limit len)))
    (setf (operating-systems page) (make-hash-table))
    (when computers
      (dolist (os (select 'operating-system :flatp t :limit len
                          :where [in [computer-id] (mapcar #'id computers)]))
        (setf (gethash (computer-id os) (operating-systems page)) os)))
    computers))
              
(defun render-computer-badge (computer open-tickets-count os)
  (<:as-html 
    (<:span :class "status" (<:as-html (if (online computer) "Online" "Offline")))
    (<:span :class "os" (<:as-html (os-shorthand os)))
    (when (> open-tickets-count 0)
      (<:span :class "tickets" (<:as-html open-tickets-count)))))

(defmethod render-item ((page computer-list) comp)
  (let ((name (name comp))
	(user (user page))
	(onlinep (online comp))
	(tags (tags comp))
	(groups (groups comp))
        (os (gethash (id comp) (operating-systems page)))
	(addresses (ip-addresses comp))
	(open-tickets-count (open-tickets-count comp))
        (contents-id (unique-id "contents-"))
        (toggler-id (unique-id "toggler-")))
    (<:tr
     :class (if (odd-row page) "odd-row" "even-row")
     (<:td :rowspan 3 :align "center" :class (format nil "~a badge" (if onlinep "online" "offline"))
	   (render-computer-badge comp open-tickets-count os))
     (<:td :class "name" 
	   (when (show-checkboxes page)
	     (let ((checkbox-id (unique-id "checkbox")))
	       (<:a :class "checkbox" :id checkbox-id
		    :onclick (format nil "selectComputer(this, '~a')" (id comp))
		    (<:as-is "&nbsp;&nbsp;&nbsp;"))))
	   (<:span (<:a :href (computer-link name) (<:ah name)))))
    (<:tr
     :class (if (odd-row page) "odd-row" "even-row")
     (<:td 
      :class "desc"
      (<:span 
       (if addresses
	   (<:ah (format nil "~{~a~#[~:;, ~]~}" (mapcar #'name addresses)))
	   (<:ah "0.0.0.0")))))
    (<:tr
     :class (format nil "action-bar ~a" (if (odd-row page) "odd-row" "even-row"))
     (<:td (<:ul
	    :class "filter-list"
	    (<:li :class "first-item" (<ucw:a :action (goto-computers-page page user :computer comp) 
					      "details"))
            (<:li
             (grey-box "New Ticket" (<:ah "new ticket")
                       (edit-ticket page (make-default-ticket user) :computer-ids (list (id comp)))
                       439 530))
	    (if (and (online comp) (has-remote-permission user comp))
		(<:li (<ucw:a
		       :action (remote-computer page comp)
		       :target "_blank"
		       :onclick
		       (format nil "var ret = confirm('Remote Desktop to the computer \"~a\"?');
                                    return ret;" (name comp))
		       "remote"))
		(<:li :class "disabled" (<:ah "remote")))
	    (if (and (online comp) (has-shutdown-permission user comp))
		(progn
		  (<:li (<ucw:a
			 :action (restart-computer page comp)
			 :onclick
			 (format nil "var ret = confirm('Reboot the computer \"~a\"?');
                                      if (ret) flashMessage('Rebooting...');
                                      return ret;" (name comp))
			 "restart"))
		  (<:li (<ucw:a
			 :action (shutdown-computer page comp)
			 :onclick
			 (format nil "var ret = confirm('Shutdown the computer \"~a\"?');
                                      if (ret) flashMessage('Shutting down...');
                                      return ret;" (name comp))
			 "shutdown")))
		(progn
		  (<:li :class "disabled" (<:ah "restart"))
		  (<:li :class "disabled"  (<:ah "shutdown"))))
	    (if (or (online comp) (not (has-note-permission user comp)))
		(<:li :class "disabled" (<:ah "delete"))
		(<:li (<ucw:a
		       :action (remove-computer page comp)
		       :onclick
		       (format nil "var ret = confirm('Remove the computer \"~a\"?');
                              if (ret) flashMessage('Removing computer...');
                              return ret;" (name comp))
		       (<:ah "delete")))))))
    (<:tr (<:td (<:br)) (<:td))))

(defaction remove-computer ((page paragent-component) computer)
  (remove-computer% page computer)
  (goto-computers-page page (user page)))
          
(defun remove-computer% (page computer)
  (let ((computer-id (id computer))
	(computer-name (name computer))
	(current-user (user page)))
    (with-db
	(let ((users (select 'user :flatp t
				   :where [= [company-id] (company-id current-user)])))
	  (dolist (user users)
	    (let ((recent-computers (recent-computers user)))
	      (setf (recent-computers user)
		    (remove computer-name recent-computers :test #'equal))
	      (clsql:update-records-from-instance user))))
      (clsql:update-instance-from-records current-user)

      (macrolet ((delete-props (&body tables)
                     `(progn
                        ,@(mapcar
                            (lambda (table)
                              `(clsql:delete-records :from ,table :where [= [computer-id] computer-id]))
                            tables))))
        (clsql:delete-records :from "computers" :where [= [id] computer-id])
        (let ((event-ids (select [slot-value 'event 'id] :from [events] :flatp t
                                 :where [= [slot-value 'event 'computer-id] computer-id])))
          (when event-ids
            (clsql:delete-records :from [alert-event-link]
                                  :where [in [slot-value 'alert-event-link 'event-id] event-ids])))
        (delete-props "group_computer_links" "alert_computer_link" "software_computer_link" "software_event"
                      "events" "computer_tag" "bios" "cd_roms" "hard_drives"
                      "logical_drives" "hardware_errors" "hotfixes" "ip_address"
                      "memory" "memory_arrays" "network_cards" "printers" "processors" "sound_devices"
                      "startups" "service" "motherboards" "user_accounts"
                      "video-controllers" "ticket_computers")))))

(defun remove-computers% (page computers)
  (declare (ignore page))
  (when computers
    (let ((computer-ids (mapcar #'id computers)))
      (with-db
        (macrolet ((delete-props (&body tables)
                     `(progn
                        ,@(mapcar
                            (lambda (table)
                              `(clsql:delete-records :from ,table :where [in [computer-id] computer-ids]))
                            tables))))
          (clsql:delete-records :from "computers" :where [in [id] computer-ids])
          (let ((event-ids (select [slot-value 'event 'id] :from [events] :flatp t
                                 :where [in [slot-value 'event 'computer-id] computer-ids])))
          (when event-ids
            (clsql:delete-records :from [alert-event-link]
                                  :where [in [slot-value 'alert-event-link 'event-id] event-ids])))
          (delete-props "group_computer_links" "alert_computer_link" "software_computer_link" "software_event"
                        "events"  "computer_tag" "bios" "cd_roms" "hard_drives"
                        "logical_drives" "hardware_errors" "hotfixes" "ip_address"
                        "memory" "memory_arrays" "network_cards" "printers" "processors" "sound_devices"
                        "startups" "service" "motherboards" "user_accounts" "video-controllers"
                        "ticket_computers"))))))

(defun connection-error-message (name)
  (format nil "Unable to contact ~a at this time. We are sorry for the inconvenience. Please try again in a moment. If you continue to receive this error, please contact support@paragent.com. Thank you." name))

(defaction restart-computer ((page paragent-component) computer)
  (if (send-archon computer (format nil "(restart ~A)" (id computer)))
      (goto-computers-page page (user page) :computer computer)
      (goto-computers-page 
       page (user page) 
       :computer computer
       :error-message (connection-error-message (name computer)))))

(defaction shutdown-computer ((page paragent-component) computer)
  (if (send-archon computer (format nil "(shutdown ~A)" (id computer)))
      (goto-computers-page page (user page) :computer computer)
      (goto-computers-page 
       page (user page) 
       :computer computer
       :error-message (connection-error-message (name computer)))))

(defaction remote-computer ((page paragent-component) computer)
  (let ((name (name computer))
	(password (random-password 64)))
    (log-remote (user page) computer)
    (if (send-archon computer 
		     (format nil "(activate-dark-templar ~A ~S)" (id computer) password))
	(call-component nil (make-instance 'observer-page 
					   :computer (name computer) 
					   :password password 
					   :user (user page)))
	(goto-computers-page 
	 page (user page) 
	 :computer computer
	 :error-message (connection-error-message (name computer))))))

(defun log-remote (user computer)
  (declare (user user))
  (declare (computer computer))
  (with-db
   (update-records-from-instance
    (make-instance 'event :company-id (company-id user) :computer-id (id computer)
                   :summary "Remote desktop used"
                   :description (format nil "~a remoted into ~a from ~a."
                                        (username user) (name computer) (ucw::peer-address (context.request *context*)))
                   :severity-id 10 :type-id +event-remote-desktop+
                   )))
  )

(defun render-tags (tags)
  (let ((ret ""))
    (dolist (tag tags)
      (setf ret (concatenate 'string ret (name tag) " ")))
    ret))

(defgeneric save-tags (computer tag-string))

(defmethod save-tags ((computer computer) tag-string)
  (clsql:with-transaction 
    ()
    (clsql:delete-records :from [computer-tag] :where [= [computer-id] (id computer)])
    (slot-makunbound computer 'tags)
    (dolist (tag (remove-duplicates (split-sequence:split-sequence #\space tag-string :remove-empty-subseqs t)
                                    :test #'string=))
      (clsql:update-records-from-instance
        (make-instance 'computer-tag
                       :computer-id (id computer)
                       :name tag)))))

#.(clsql:restore-sql-reader-syntax-state)
