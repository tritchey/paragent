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

(defun readable-byte-size (size)
  (declare (type (or null number) size))
  (if size
      (if (>= size 1073741824)
          (format nil "~$ GB" (* 9.31322575d-10 size))
          (format nil "~$ MB" (* 9.53674316d-7 size)))
      "unknown"))

(defun readable-kbyte-size (size)
  (declare (type (or null number) size))
  (if size
      (readable-byte-size (* size 1024))
      "unknown"))

(defun yes-no (val)
  (if val
      "Yes"
      "No"))

(tabbed-event-list computer-event-list inline-event-list 1)
(tabbed-event-list computer-ticket-list inline-ticket-list 2)

(defaction do-assigned-filter :after ((page computer-ticket-list) filter)
  (setf (current-tab (tabbed-view page)) 2))
(defaction do-tag-filter :after ((page computer-ticket-list) filter)
  (setf (current-tab (tabbed-view page)) 2))

(defcomponent computer-tabbed-view (tabbed-view)
  ((computer :accessor computer
             :initarg :computer
             :type computer)
   (event-list :accessor event-list
	       :initarg :event-list
	       :type computer-event-list)
   (ticket-list :accessor ticket-list
                :initarg :ticket-list
                :type computer-ticket-list))
  (:documentation "Displays all the properties of a computer.")
  (:default-initargs
      :toolbar #'render-computer-toolbar
      :contents (list #'render-computer-events 
		      #'render-computer-tickets
		      #'render-computer-hardware 
		      #'render-computer-software)
      :tabs (list "Events" "Tickets" "Hardware" "Software")))

(defmethod initialize-instance :after ((page computer-tabbed-view) &key title)
  (declare (ignore title))
  (with-db
    (setf (event-list page)
          (make-instance 'computer-event-list 
                         :user (user page)
                         :condensed t
                         :tabbed-view page))
    (set-filter (event-list page) :event
		[= [computer-id] (id (computer page))] [events])
    
    (setf (ticket-list page)
          (make-instance 'computer-ticket-list 
			 :user (user page) 
			 :condensed t
			 :tabbed-view page))
    (set-filter (ticket-list page)
                :computer
                [and
                [= [slot-value 'ticket 'id] [slot-value 'ticket-computer 'ticket-id]]
                [= [slot-value 'ticket-computer 'computer-id] (id (computer page))]]
                [ticket-computers])))

(defcomponent computer-page (paragent-window-component)
  ((computer :accessor computer
             :initarg :computer
             :type computer)
   (tabbed-view :accessor tabbed-view
		:initarg tabbed-view
		:type computer-tabbed-view)
   (error-message :accessor error-message
		  :initarg :error-message
		  :initform nil)))

(defmethod initialize-instance :after ((page computer-page) &key title)
  (declare (ignore title))
  (push "css/gsearch.css" (window-component.stylesheet page))
  (setf (tabbed-view page)
	(make-instance 'computer-tabbed-view 
		       :computer (computer page) 
		       :user (user page)
	               :place (make-place (tabbed-view page)))))

(defgeneric render-computer (page computer)
  (:documentation "Displays all the properties of the computer"))

(defmethod body-id ((component computer-page))
  "computers")

(defmethod render-computer ((page computer-page) computer)
  (declare (type computer computer))
  (show-tip (user page)
            "If you hover over an item, icons will appear allowing you to search either for all computers with that same item, or without it.")
  (render (tabbed-view page)))

(defrender ((page computer-page))
  (init-grey-box)
  (render-computer page (computer page))
  (when (error-message page)
    (<:script :type "text/javascript"
	      (<:ai (format nil "alert(\"~a\")" (error-message page)))))
  (when *google-search-enabled-p*
    (import-google-search-api)
    (<:div :id "search-control" :class "floating-tooltip-border"
           :style "display:none;"
           :onmouseover "keepTooltip('search-control');"
           :onmouseout "hideTooltip('search-control');"
           (<:div :class "floating-tooltip"
                  (<:div :class "title-bar"
                         (left-right-align
                           (<:strong "More information")
                           (<:a :class "close-button" :href "#"
                                :onclick "javascript:hideTooltip('search-control'); return false;"
                                (<:img :src "images/close.gif"))))
                  (<:div :id "search-contents")))
    (<:script
      :type "text/javascript"
      (<:ai "var searchControl = new GSearchControl();
            
            // Add in a full set of searchers
            searchControl.addSearcher(new GwebSearch());
            
            // Tell the searcher to draw itself and tell it where to attach
            searchControl.draw(document.getElementById('search-contents'));
            
            // Execute an inital search
            //searchControl.execute('Google');
            "))))

(defmethod render-recent-computers ((page computer-page))
  ;; first, update the recent-computers list for this user
  (with-db
      (let* ((user (user page))
	     (name (name (computer page)))
	     (recent-list (recent-computers user)))
        (setf recent-list (remove-if #'(lambda (item) (string= name item)) recent-list)) 
	(push (name (computer page)) recent-list)
	(setf (recent-computers user) (if (> (length recent-list) 10)
					      (subseq recent-list 0 10)
					      recent-list))
	(clsql:update-records-from-instance user)))
  ;; now render the tasks
  (let ((user (user page)))
    (<:h1 "Recent Computers")
    (<:ul
     (<:li (<:a :href "computers.ucw" "All Computers"))
     (if (recent-computers user)
	 (dolist (name (recent-computers user))
	     (<:li (<:a :href (computer-link name) (<:ah name))))
	 (<:li (<:ah "(Empty)"))))))


(defaction edit-computer ((page paragent-component) computer)
  (call 'edit-computer-dialog :user (user page) :computer computer))


(defaction cancel-edit ((page computer-tabbed-view))
  (goto-dialog-confirm page "canceling." (computer-link (computer page))))

(defaction cancel-edit ((page paragent-component))
  (answer t))

(defaction save-computer-metadata ((page paragent-component) computer tags warranty-field name)
  (save-computer-metadata% page computer tags warranty-field name)
  (goto-dialog-confirm page "Computers added." (computer-link computer)))

(defaction save-computer-metadata ((page computer-tabbed-view) computer tags warranty-field name)
  (save-computer-metadata% page computer tags warranty-field name))

(defaction save-computer-metadata ((page computer-page) computer tags warranty-field name)
  (save-computer-metadata% page computer tags warranty-field name)
  (answer t))

(defgeneric save-computer-metadata% (page computer tags warranty-field name))

(defmethod save-computer-metadata% ((page paragent-component) computer tags warranty-field name)
  (with-db
   (save-tags computer tags)
   (when (value warranty-field)
     (setf (warranty computer) (clsql-sys::time->date (clsql:utime->time (value warranty-field)))))
   (let ((user (user page)))
     (setf (recent-computers user) (remove-if #'(lambda (item) (string= (name computer) item)) 
                                              (recent-computers user)))
     (update-records-from-instance user))
   (setf (alias computer) name)
   (update-records-from-instance computer)
   (update-instance-from-records computer)
   (update-objects-joins (list computer))))

(defmacro search-query (prop class)
  "Creates a function that takes the object to search on and constructs
    the appropriate piece of the query"
  `(lambda (obj)
     (with-db
       (let ((allowed (select [slot-value ',class 'computer-id]
			      :flatp t :from (table-for-object ',class)
			      :where
			      [and
			      ,@(if (listp prop)
				    (mapcar
				      (lambda (p) `[= [slot-value ',class ',p] (,p obj)])
				      prop)
				    `([= [slot-value ',class ',prop] (,prop obj)]))])))
	 (if allowed
	     [in [slot-value 'computer 'id] allowed]
	     [= 1 1])))))

(defmacro not-search-query  (prop class)
  "Creates a function that generates the query to search for computers
   that DO NOT have the given object"
  `(lambda (obj)
     (with-db
       (let ((forbidden (select [slot-value ',class 'computer-id]
				:flatp t :from (table-for-object ',class)
				:where
				[and
				,@(if (listp prop)
				      (mapcar
					(lambda (p) `[= [slot-value ',class ',p] (,p obj)])
					prop)
				      `([= [slot-value ',class ',prop] (,prop obj)]))])))
	 (if forbidden
	     [not [in [slot-value 'computer 'id] forbidden]]
	     [= 1 1])))))

(defmacro search-desc (format-string &rest prop)
  "Makes a function that, when given the object being searched on,
   returns a string describing the search"
  `(lambda (obj)
     (format nil ,format-string ,@(mapcar
                                    (lambda (p) `(,p obj))
                                    prop))))


(defgeneric render-computer-toolbar (page)
  (:documentation "render the toolbar for the computer details page"))

(defun os-shorthand (os)
  (declare (type (or null operating-system) os))
  (if os
      (let ((os (name os))
	    (sp (service-pack os)))
	(cond
	  ((search "XP" os)
	   (format nil "XP~A" (cond
				 ((string= sp "Service Pack 2")
				  " SP2")
				 ((string= sp "Service Pack 1")
				  " SP1")
				 (t ""))))
	  ((search "Vista" os)
	   "Vista")
	  ((search "2000" os)
	   "2000")
	  ((search "2003" os)
	   "2003")
	  ((search "OS X" os)
	   "OS X")
	  ((search "Linux" os)
	   "Linux")
	  (t
	   os)))
      "Unknown"))

(defmethod render-computer-toolbar ((page computer-tabbed-view))
  (let* ((comp (computer page))
	 (name (name comp))
         (unaliased-name (unaliased-name comp))
	 (user (user page))
	 (onlinep (online comp))
	 (tags (render-tags (tags comp)))
	 (tag-id (unique-id "tags"))
	 (note (note comp))
	 (warranty-field (make-instance 'calendar-date-field))
	 (warranty (warranty comp))
	 (groups (groups comp))
	 (addresses (ip-addresses comp))
	 (antivirus (car (antivirus comp)))
	 (firewall (car (firewall comp)))
	 (memory (memory comp))
	 (open-tickets (open-tickets comp))
	 (contents-id (unique-id "contents-"))
	 (toggler-id (unique-id "toggler-")))
    (with-db
      (<:script :src "actb/actb.js" :type "text/javascript")
      (<:script :src "actb/common.js" :type "text/javascript")
      (clsql:update-instance-from-records comp)
      (when warranty
	(multiple-value-bind (day month year) (decode-date warranty)
	  (setf (value (ucw::month warranty-field)) month)
	  (setf (value (ucw::day warranty-field)) day)
	  (setf (value (ucw::year warranty-field)) year)))
      (<ucw:form
       :id "edit-computer-details"
       :action (save-computer-metadata page comp tags warranty-field name)
       (<:table
	:id "comppick" :cellspacing 5
	(<:tr
	 (<:td :rowspan 10 :align "center" :class (format nil "~a badge" (if onlinep "online" "offline"))
	       (render-computer-badge comp (length open-tickets) (car (operating-system comp))))
	 (<:td :class "name"
	       :colspan 2
	       (<:p :id "static-name" (<:ah name))
	       (<:p :id "edit-name" :style "display:none" (<ucw:text :accessor name))))
        (when (not (equal name unaliased-name))
          (<:tr (<:td :class "title" 
                      (<:p "Real Name"))
	      (<:td :class "desc" (<:ah unaliased-name))))
	(<:tr (<:td :class "title" 
		    (<:p "Address"))
	      (<:td :class "desc"
		    (if addresses
			(<:ah (format nil "~{~a~#[~:;, ~]~}" (mapcar #'name addresses)))
			(<:ah "0.0.0.0"))))
        (when antivirus
          (<:tr (<:td :class "title" 
                      (<:p "Antivirus"))
                (<:td :class "desc"
                      (if antivirus
                          (<:p
                           (<:ah (format nil "~a " 
                                         (name antivirus))) 
                           (if (up-to-date antivirus) 
                               (<:ah "Up-to-date")
                               (<:span :style "color: #e0490e;" (<:ah "Out-of-date"))))
                          (<:i "(No Information)")))))
        (when firewall
          (<:tr (<:td :class "title" 
                      (<:p "Firewall"))
                (<:td :class "desc"
                      (if firewall
                          (<:p
                           (<:ah (format nil "~a "  (name firewall)))
                           (if (enabled firewall) 
                               (<:ah "Enabled")
                               (<:span :style "color: #e0490e;" (<:ah "Disabled"))))
                          (<:i "(No Information)")))))
        (let ((mem-capacity (reduce (lambda (&optional a b)
                                      (if (and a b)
                                          (+ a b)
                                          (or a b)))
                                    (mapcar #'capacity memory))))
          (when mem-capacity
            (<:tr (<:td :class "title" 
                        (<:p "Memory"))
                  (<:td :class "desc"
                        (<:p
                         (<:ah (format nil "~a"  
                                       (readable-byte-size 
                                        mem-capacity))))))))
        (when groups
          (<:tr (<:td :class "title" 
                      (<:p "Groups"))
                (<:td :class "desc"
                      (if groups
                          (<:ah (format nil "~{~a~#[~:;, ~]~}" (mapcar #'name groups)))
                          (<:i "(None)")))))
	(<:tr (<:td :class "title" 
		    (<:p "Tags"))
	      (<:td :class "desc"
		    (<:p :id "static-tags"
			 (if (not-blank tags)
			     (<:ah tags)
			     (<:i "(None)")))
		    (<:p :id "edit-tags"
			 :style "display: none;"
			 (<ucw:text
			  :accessor tags 
			  :class "tags" 
			  :id tag-id
			  :onfocus (format nil "javascript:~a"
					   (js:js-to-string
					    `(actb ($ ,tag-id)
						   (list ,@(get-tags (company (user page)))))))))))
	(<:tr (<:td :class "title" 
		    (<:p "Note"))
	      (<:td :class "desc"
		    (<:p :id "static-note"
			 (if (not-blank note)
			     (<:ah note)
			     (<:i "(None)")))
		    (<:p :id "edit-note"
			 :style "display: none;"
			 (<ucw:textarea :class "note" :accessor (note comp) :cols 55 :rows 3))))
	(<:tr (<:td :class "title" 
		    (<:p "Warranty"))
	      (<:td :class "desc"
		    (<:p :id "static-warranty"
			 (if warranty
			     (<:ah (print-date (clsql-sys::date->time warranty) :long-day))
			     (<:i "(None)")))
		    (<:p :id "edit-warranty"
			 :style "display: none;"
			 (render warranty-field))))
	(<:tr
	 :class "action-bar"
	 (<:td :colspan 2 
	       (<:ul 
		:class "filter-list"
		(if (has-note-permission user comp)
		    (<:li 
		     :id "edit"
		     :class "first-item"
		     (<ucw:a 
		      :action (edit-computer page comp)
		      :onclick "javascript:Element.hide('edit'); Element.hide('static-name'); Effect.Appear('edit-name'); Element.hide('static-tags'); Effect.Appear('edit-tags'); Element.hide('static-note'); Effect.Appear('edit-note'); Element.hide('static-warranty'); Effect.Appear('edit-warranty'); Effect.Appear('cancel'); Effect.Appear('save'); return false;"
		      "edit"))
		    (<:li :class "disabled" (<:ah "edit")))
		(<:li
		 :id "save"
		 :class "first-item"
		 :style "display: none;"
		 (<:a
		  :href "#"
		  :onclick "javascript:$('edit-computer-details').submit();"
		  "save"))
		(<:li
		 :id "cancel"
		 :style "display: none;"
		 (<ucw:a 
		  :action (cancel-edit page)
		  :onclick "javasccript:Effect.Appear('edit'); Element.hide('save'); Element.hide('cancel'); Element.hide('edit-name'); Effect.Appear('static-name'); Element.hide('edit-tags'); Effect.Appear('static-tags'); Element.hide('edit-note'); Effect.Appear('static-note'); Element.hide('edit-warranty'); Effect.Appear('static-warranty'); return false;"
		  "cancel"))
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
			     "shutdown"))
		      (<:li (<ucw:a
			     :action (restart-agent page comp)
			     :onclick
			     (format nil "var ret = confirm('Restart the Paragent agent on \"~a\"?');
                                      if (ret) flashMessage('Restarting...');
                                      return ret;" (name comp))
			     "restart agent")))
		    
		    (progn
		      (<:li :class "disabled" (<:ah "restart"))
		      (<:li :class "disabled"  (<:ah "shutdown"))
		      (<:li :class "disabled" (<:ah "restart agent"))))
		(if (or (online comp) (not (has-note-permission user comp)))
		    (<:li :class "disabled" (<:ah "delete"))
		    (progn
		      (<:li (<ucw:a
			     :action (remove-computer page comp)
			     :onclick
			     (format nil "var ret = confirm('Remove the computer \"~a\"?');
                              if (ret) flashMessage('Removing computer...');
                              return ret;" (name comp))
			     (<:ah "delete")))))))))))))

(defgeneric render-computer-events (page)
  (:documentation "Shows summary info on the computer"))

(defmethod render-computer-events ((page computer-tabbed-view))
  (let ((computer (computer page)))
    (clsql:update-instance-from-records computer)
    (render (event-list page))))

(defgeneric render-computer-tickets (page)
  (:documentation "Shows summary info on the computer"))

(defmethod render-computer-tickets ((page computer-tabbed-view))
  (render (ticket-list page)))


(defgeneric render-computer-hardware (page)
  (:documentation "Displays the hardware for the computer"))

(defmethod render-computer-hardware ((page computer-tabbed-view))
  (let ((computer (computer page)))
     (render-prop-table computer "Bios"
			  (bios computer)
			  '("Name" "Manufacturer" "Version"
			    "Serial Number")
			  '(name manufacturer version serial-number) :page page
			  :search (search-query (name manufacturer) bios)
			  :search-tables (list [bios])
			  :description (search-desc 
					"Computers with the Bios '~a' manufactured by ~a" 
					name manufacturer)
			  :not-search (not-search-query (name manufacturer) bios)
			  :not-description (search-desc 
					    "Computers without the Bios '~a' manufactured by ~a" 
					    name manufacturer)
			  :google-props '(name manufacturer)
			  :icon "images/software.gif")
    (render-prop-table computer "CD-ROMs"
			 (cd-roms computer)
			 '("Name" "Drive")
			 '(name drive) :page page
			 :search (search-query name cd-rom)
			 :search-tables (list [cd-roms])
			 :description (search-desc "Computers with CD-ROMs named ~a" name)
			 :not-search (not-search-query name cd-rom)
			 :not-description (search-desc "Computers without CD-ROMs named ~a" name)
			 :google-props '(name)
			 :icon "images/hardware.gif")
    (render-prop-table computer "Memory"
			 (memory computer)
			 '("Capacity" "Speed" "Form Factor" "Manufacturer")
			 '((capacity . readable-byte-size)  memory-speed form-factor manufacturer) 
			 :page page
					;We have to treat this special because the slot name doesn't match the accessor
			 :search 
			 (lambda (obj)
			   [and
			   [= [slot-value 'memory 'computer-id] [slot-value 'computer 'id]]
			   [= [slot-value 'memory 'speed] (memory-speed obj)]
			   [= [slot-value 'memory 'form-factor] (form-factor obj)]])
			 :description (search-desc 
				       "Computers with memory of speed ~a and a form factor of '~a'" 
				       memory-speed form-factor)
			 :search-tables (list [memory])
					;We have to treat this special because the slot name doesn't match the accessor
			 :not-search 
			 (lambda (obj)
			   [not [exists
			   [select [*] :from [memory]
			   :where [and
			   [= [slot-value 'memory 'computer-id] [slot-value 'computer 'id]]
			   [= [slot-value 'memory 'speed] (memory-speed obj)]
			   [= [slot-value 'memory 'form-factor] (form-factor obj)]]]]])
			 :not-description (search-desc 
					   "Computers without memory of speed ~a and a form factor of '~a'" 
					   memory-speed form-factor)
			 :icon "images/hardware.gif")
    (render-prop-table computer "Network Card"
			 (network-cards computer)
			 '("Name" "Mac-Address" "Manufacturer")
			 '(name mac-address manufacturer) :page page
			 :search (search-query (name manufacturer) network-card)
			 :search-tables (list [network-cards])
			 :description (search-desc 
				       "Computers with a network card named '~a' manufactured by '~a'" 
				       name manufacturer)
			 :not-search (not-search-query (name manufacturer) network-card)
			 :not-description (search-desc 
					   "Computers without a network card named '~a' manufactured by '~a'" 
					   name manufacturer)
			 :google-props '(name manufacturer)
			 :icon "images/hardware.gif")
    (render-prop-table computer "Printers"
			 (printers computer)
			 '("Name" "Default Printer?")
			 '(name is-default) :page page
			 :search (search-query name printer)
			 :search-tables (list [printers])
			 :description (search-desc "Computers with the printer '~a' installed" name)
			 :not-search (not-search-query name printer)
			 :not-description (search-desc "Computers without the printer '~a' installed" name)
			 :google-props '(name)
			 :icon "images/hardware.gif")
    (render-prop-table computer "Processors"
			 (processors computer)
			 '("Name" "Info" "Architecture" "L2 Cache" "Clock Speed")
			 '(name info architecture l2-cache clock-speed) :page page
			 :search (search-query (name architecture clock-speed) processor)
			 :search-tables (list [processors])
			 :description (search-desc 
				       "Computers with a processor named '~a', a clockspeed of ~a and ~a architecture" 
				       name clock-speed architecture)
			 :not-search (not-search-query (name architecture clock-speed) processor)
			 :not-description (search-desc 
					   "Computers without a processor named '~a', a clockspeed of ~a and ~a architecture" 
					   name clock-speed architecture)
			 :google-props '(name)
			 :icon "images/hardware.gif")
    (render-prop-table computer "Physical Drives"
                       (hard-drives computer)
                       '("Name" "Size" "Interface Type")
                       '(name (size . readable-byte-size) interface-type)
                       :page page
                       :search (search-query (name size) hard-drive)
                       :search-tables (list [hard-drives])
                       :description (search-desc "Computers with a physical drive named ~a and sized ~a" name size)
                       :not-search (not-search-query (name size) hard-drive)
                       :not-description (search-desc "Computers without a physical drive named ~a and sized ~a" name size)
                       :google-props '(name)
		       :icon "images/hardware.gif")
    (render-prop-table computer "Hard Drives"
                       (logical-drives computer)
                       '("Name" "Size" "Free Space")
                       '(name (size . readable-byte-size) (free-space . readable-byte-size))
                       :page page
                       :search (search-query (name size) logical-drive)
                       :search-tables (list [logical-drives])
                       :description (search-desc "Computers with a hard drive named ~a and sized ~a" name size)
                       :not-search (not-search-query (name size) logical-drive)
                       :not-description (search-desc "Computers without a hard drive named ~a and sized ~a" name size)
		       :icon "images/hardware.gif")
    (render-prop-table computer "Hardware Errors"
                       (hardware-errors computer)
                       '("Description")
                       '(description) :page page
                       :search (search-query description hardware-error)
                       :search-tables (list [hardware-errors])
                       :description (search-desc "Computers with the hardware error '~a'" description)
                       :not-search (not-search-query description hardware-error)
                       :not-description (search-desc "Computers without the hardware error '~a'" description)
                       :icon "images/hardware.gif")
    (render-prop-table computer "Sound Devices"
                       (sound-devices computer)
                       '("Name" "Manufacturer")
                       '(name manufacturer) :page page
                       :search (search-query (name manufacturer) sound-device)
                       :search-tables (list [sound-devices])
                       :description (search-desc "Computers with the sound device '~a' manufactured by '~a'" name manufacturer)
                       :not-search (not-search-query (name manufacturer) sound-device)
                       :not-description (search-desc "Computers without the sound device '~a' manufactured by '~a'" name manufacturer)
                       :google-props '(name manufacturer)
                       :icon "images/hardware.gif")
    (render-prop-table computer "Motherboard"
                       (motherboards computer)
                       '("Name" "Manufacturer" "Serial Number")
                       '(name manufacturer serial-number) :page page
                       :search (search-query (name manufacturer) motherboard)
                       :search-tables (list [motherboards])
                       :description (search-desc "Computers with the motherboard '~a' manufactured by '~a'" name manufacturer)
                       :not-search (not-search-query (name manufacturer) motherboard)
                       :not-description (search-desc "Computers without the motherboard '~a' manufactured by '~a'" name manufacturer)
                       :google-props '(name manufacturer)
                       :icon "images/hardware.gif")
    (render-prop-table computer "Video Controller"
                       (video-controllers computer)
                       '("Name" "Horizontal Resolution" "Vertical Resolution"
                                "Refresh Rate" "Driver")
                       '(name horizontal-resolution vertical-resolution
                              refresh-rate driver) :page page
                       :search (search-query (name driver) video-controller)
                       :description (search-desc "Computers with the video controller '~a' using the driver '~a'" name driver)
                       :search-tables (list [video-controllers])
                       :not-search (not-search-query (name driver) video-controller)
                       :not-description (search-desc "Computers without the video controller '~a' using the driver '~a'" name driver)
                       :google-props '(name)
                       :icon "images/hardware.gif")))


(defgeneric render-computer-software (page)
  (:documentation "Renders the software on this computer"))

(defmethod render-computer-software ((page computer-tabbed-view))
  (let ((computer (computer page)))
    (render-prop-table computer "Operating System"
                       (operating-system computer)
                       '("Name" "Version" "License Key" "Product ID" "Registered User" "Service Pack")
                       `(name version (license-key . ,(lambda (x)
                                                       (if (guest-p (user page))
                                                           "XXXXX-XXXXX-XXXXX-XXXXX-XXXXX"
                                                           (if x x "Unknown"))))
                                                        product-id registered-user service-pack) :page page
                       :search (search-query (name version) operating-system)
                       :search-tables (list [operating-systems])
                       :description (search-desc "Computers with the operating system '~a' version ~a" name version)
                       :not-search (not-search-query (name version) operating-system)
                       :not-description (search-desc "Computers without the operating system '~a' version ~a" name version)
                       :google-props '(name)
                       :icon "images/computer.gif")
    (render-prop-table computer "Hot Fixes"
                       (remove-if
                         (lambda (hot-fix) (string= (name hot-fix) "File 1"))
                         (hot-fixes computer))
                       '("Name" "Installed By")
                       '(name installed-by) :page page
                       :search (search-query name hot-fix)
                       :search-tables (list [hotfixes])
                       :description (search-desc "Computers with the hot fix '~a'" name)
                       :not-search (not-search-query name hot-fix)
                       :not-description (search-desc "Computers without the hot fix '~a'" name)
                       :google-props '(name)
                       :custom-search #'link-for-hotfix
                       :icon "images/software.gif")
    (render-prop-table computer "User Accounts"
		       (user-accounts computer)
		       '("Name" "Disabled")
		       '(name (disabled . yes-no)) :page page
		       :search (search-query name user-account)
		       :description (search-desc "Computers with the user account '~a'" name)
		       :search-tables (list [user-accounts])
		       :not-search (not-search-query name user-account)
		       :not-description (search-desc "Computers without the user account '~a'" name)
		       :icon "images/user.gif")
    (render-prop-table computer "Services"
		       (services computer)
		       '("Name")
		       '(name)
		       :page page
		       :search (search-query (name) service)
		       :description (search-desc "Computers with the service ~a" name)
		       :search-tables (list [service])
		       :not-search (not-search-query (name) service)
		       :not-description (search-desc "Computers without the service ~a" name)
		       :google-props '(name)
		       :google-extra-terms '("Service")
                       :icon "images/service.gif")
    (render-prop-table computer "Installed Software"
		       (sort (software computer) #'name<)
		       '("Name" "Publisher" "Version")
		       '(name publisher version) :page page
		       :search
		       (lambda (obj)
			 [and [= [slot-value 'computer 'id] [slot-value 'software-computer-link 'computer-id]]
			 [= [slot-value 'software-computer-link 'software-id] (software-id obj)]])
		       :description (search-desc "Computers with the software '~a' installed" name)
		       :search-tables (list [software-computer-link])
		       :not-search
		       (lambda (obj)
			 [not [exists
			 [select [slot-value 'software-computer-link 'id]
			 :from (list [software-computer-link])
			 :where [and
			 [= [slot-value 'software-computer-link 'software-id] (software-id obj)]
			 [= [slot-value 'software-computer-link 'computer-id] [slot-value 'computer 'id]]]]]])
		       :not-description (search-desc "Computers without the software '~a' installed" name)
		       :google-props '(name)
		       :icon "images/software.gif")))

(defun render-prop-table (computer title objs columns props &key page search description search-tables
                                   not-search not-search-tables not-description
                                   google-props google-extra-terms custom-search custom-search-prop
				   icon)
  "Does all the work to render properties in a consistent manner"
  (declare (type computer computer))
  (declare (string title))
  (declare (list objs columns props))
  
  ;(setf search-tables nil)
  ;(setf not-search-tables nil)
  (when objs
    (let* ((id (concatenate 'string (name computer) title))
           (excerpt-id (concatenate 'string id "excerpt"))
           (full-id (concatenate 'string id "full")))
      (macrolet ((render-prop ()
                   `(let* ((prop-func (if (listp prop) (car prop) prop))
                           (val (funcall prop-func obj)))
                      (<:td 
                        (if (find prop-func google-props)
			    (<:a :href (if custom-search
					   (funcall custom-search val)
					   (format nil "http://www.google.com/search?q='~a' ~{~a ~}" val google-extra-terms))
				 :target "_blank"
				 :class "tip-raiser"
				 :onmouseover (if *google-search-enabled-p*
						  (js:js-to-string `(search-tooltip this "search-control"
										    ,(format nil "~a ~{~a ~}" val google-extra-terms)))
						  "")
				 :onmouseout (if *google-search-enabled-p*
						 "hideTooltip('search-control');"
						 "")
				 (<:as-html (cond
					      ((listp prop)
					       (funcall (cdr prop) val))
					      ((eql val nil) "unknown")
					      ((eql val t) "Yes")
					      (t val))))
			    (if (find prop-func custom-search-prop)
				(<:a :href (if custom-search
					   (funcall custom-search obj)
					   (format nil "http://www.google.com/search?q='~a' ~{~a ~}" val google-extra-terms))
				 :target "_blank"
				 :class "tip-raiser"
				 (<:as-html (cond
					      ((listp prop)
					       (funcall (cdr prop) val))
					      ((eql val nil) "unknown")
					      ((eql val t) "Yes")
					      (t val))))
				(<:as-html (cond
					     ((listp prop)
					      (funcall (cdr prop) val))
					     ((eql val nil) "unknown")
					     ((eql val t) "Yes")
					     (t val))))))))
                 (render-row
                   ()
                   `(let ((img-id (unique-id "search"))
                          (img-id2 (unique-id "search-without"))
                          (blank-id (unique-id "blank")))
                      (<:tr
                        :onmouseover (if search
					 (format nil "this.className='search-row'; Element.hide('~a');Element.show('~a');Element.show('~a')" blank-id img-id img-id2)
					 "")
			:onmouseout (if search
					(format nil "this.className=''; Element.hide('~a');Element.hide('~a');Element.show('~a');" img-id img-id2 blank-id)
					"")
                        (let ((obj obj))
                          (<:td :class "search"
				:width "50px"
                                (if (and search not-search)
                                    (progn
                                      (<:img :src "images/blank.png" :id blank-id :class "blank" :alt "")
                                      (<ucw:a :action (do-search page (funcall search obj) search-tables
                                                                 (funcall description obj))
                                              :onclick "flashMessage('Searching...');"
                                              (<:img :src "images/search.gif" :style "display:none;"
                                                     :alt "Search" :id img-id :class "search-with"))
                                      (<ucw:a :action (do-search page (funcall not-search obj) search-tables
                                                                 (funcall not-description obj))
                                              :onclick "flashMessage('Searching...');"
                                              (<:img :src "images/search-without.gif" :style "display:none"
                                                     :alt "Search" :id img-id2 :class "search-without")))
                                    (<:img :src "images/blank.png" :class "blank" :alt ""))))
                        (dolist (prop props)
                          (render-prop))))))
	(<:div
          :class "computer-property"
          (<:h3
	    (when icon (<:img :src icon))
	    (<:as-html title)))
        (if (null objs)
            (<:div :class "comp-prop-none" (<:as-html "No Entry"))
            (let* ((len (length objs))
                   (shorten (> len 5)))
              (progn
                (<:div
                  :id excerpt-id :class "comp-prop-table"
                  (<:table
                    :border 0 :cellspacing 0
                    (<:thead
                      (<:tr
                        (<:th :class "search" (<:img :src "images/blank.jpeg" :alt "" :height 1 :width 15))
                        (dolist (column columns)
                          (<:th (<:as-html column)))))
                    (do ((i 0 (1+ i))
                         (remainder (cdr objs) (cdr remainder))
                         (obj (car objs) (car remainder)))
                      ((or (>= i 5) (not obj)))
                      
                      (render-row))
                    (if shorten
                        (<:tr
                          (<:td)
                          (<:td
                            (<:a :onclick (format nil "javascript:var hidee = $('~a'); var showee = $('~a');
                                                  Element.toggle(hidee);
                                                  Element.toggle(showee);
                                                  return false;"
                                                  excerpt-id full-id)
                                 :href "#"
                                 
                                 (<:ai "&nabla; ") (<:as-html (- (length objs) 5)) " more"))))))
                (if shorten
                    (<:div
                      :id full-id :class "comp-prop-table" :style "display:none"
                      (<:table
                        :id full-id :border 0 :cellspacing 0
                        (<:thead
                          (<:tr
                            (<:th :class "search" (<:img :src "images/blank.png" :alt "" :height 1 :width 15))
                            (dolist (column columns)
                              (<:th (<:b (<:as-html column))))))
                        (dolist (obj objs)
                          (render-row))
                        (<:tr
                          (<:td)
                          (<:td
                            (<:a :onclick (format nil "javascript:Element.toggle('~a');Element.toggle('~a'); return false;"
                                                  excerpt-id full-id)
                                 :href "#"
                                 (<:ah "Hide"))))))))))))))

#.(clsql:restore-sql-reader-syntax-state)
