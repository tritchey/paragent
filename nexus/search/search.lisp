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

(defaction goto-search ((page paragent-component) &key (search ""))
  (call 'search-page :user (user page) :search-clause search))

(defaction do-search ((page paragent-component) search-sql search-tables description)
  (call-component nil (do-search% page search-sql search-tables description)))

(defgeneric do-search% (page search-sql search-tables description))

(defmethod do-search% ((page paragent-component) search-sql search-tables description)
  (let* ((user (user page))
         (search-page (make-instance 'search-page :user user
                                     :description description
                                     :show-computers nil ;prevent us from initializing the computer-list with useless data
                                     :show-events nil :show-software nil :show-tickets nil))
         (computer-list (make-instance 'computer-list :user user :max-results 20
                                       :place (make-place (computer-list search-page))
                                       :use-optimized t :none-message "No results found")))
    (setf (computer-list search-page) computer-list)
    (setf (default-view-classes computer-list) search-tables)
    (set-filter (computer-list search-page) :search search-sql [computers])
    (setf (show-computers search-page) t)
    search-page))



;;; The actual search results page


(defcomponent search-page (paragent-window-component)
  ((search-clause :accessor search-clause
                  :type string
                  :initarg :search-clause
                  :initform "")
   (description :accessor description
                :type string
                :initarg :description
                :initform "")
   (list-getter :accessor list-getter
                :initarg :list-getter)
   (computer-list :accessor computer-list
                  :type computer-list)
   (event-list :accessor event-list
               :type event-list)
   (software-list :accessor software-list
                  :type software-list)
   (ticket-list :accessor ticket-list
                :type ticket-list)
   (show-computers :accessor show-computers
                   :type boolean
                   :initarg :show-computers
                   :initform t)
   (show-events :accessor show-events
                :type boolean
                :initarg :show-events
                :initform t)   
   (show-software :accessor show-software
                  :type boolean
                  :initarg :show-software
                  :initform t)
   (show-tickets :accessor show-tickets
                 :type boolean
                 :initarg :show-tickets
                 :initform nil))
  (:default-initargs
   :title "Search"))



(defmethod initialize-instance :after ((page search-page) &key)
  (let ((user (user page))
        (search-clause (format nil "%~a%" (search-clause page))))
    (when (show-computers page)
      (setf (computer-list page)
            ;(make-instance 'computer-list :user user :max-results 20 :show-header t
            ;               :place (make-place (computer-list page))))
            (make-instance
              'search-computer-list :user user :max-results 20 :place (make-place (computer-list page))
              :search-clause search-clause :none-message "No matching computers found."
              )))
    (when (show-events page)
      (setf (event-list page)
            (make-instance 'event-list :user user :max-results 20 :div-id "latest-large"
                           :none-message "No matching events found."
                           :place (make-place (computer-list page))))
      (set-filter (event-list page) :search
                  [or [like [slot-value 'event 'summary] search-clause]
                  [like [slot-value 'event 'description] search-clause]]
                  [events]))
    
    (when (show-software page)
      (setf (software-list page)
            (make-instance 'software-list :user (user page) :show-header t
                           :none-message "No matching software found."
                           :place (make-place (computer-list page))))
      (set-filter (software-list page) :search [like [slot-value 'software 'name] search-clause] [software]))
    
    (when (show-tickets page)
      (setf (ticket-list page)
            (make-instance 'ticket-list :user (user page)
                           :none-message "No matching events found."
                           :place (make-place (ticket-list page))))
      (set-filter (ticket-list page) :search
                  [or
                  [like [slot-value 'ticket 'subject] search-clause]
                  [like [slot-value 'ticket 'body] search-clause]]
                  [tickets]))
    ))



(defrender ((page search-page))
  (<:script :src "actb/actb.js" :type "text/javascript")
  (<:script :src "actb/common.js" :type "text/javascript")
  (let ((search-clause (search-clause page)))
    (<:fieldset 
     :class "search"
     (<:form :method "GET" :action "search.ucw"
	     (<:text :name "q" :value search-clause)
	     (<:input :type "image" :src "images/searchbtn.gif" :class "button"))
     (<:p (link-to-advanced-search "Advanced Search")))
    (<:h3 :class "search" (if (and (equal search-clause "") 
				   (not (equal (description page) "")))
              (<:as-html (description page))
              (<:as-html "Results for '" search-clause "'")))
    
    (when (show-computers page)
      (<:br)
      (render (computer-list page)))
    (when (show-events page)
      (<:br)
      (render (event-list page)))
    (when (show-software page)
      (<:br)
      (render (software-list page)))
    (when (and (show-tickets page) (>=account-plus (company (user page))))
      (<:br)
      (render (ticket-list page)))
    ))




;;; We have our own special computer-list here, because the filters just don't query quickly enough

(defcomponent search-computer-list (computer-list)
  ((search-clause :accessor search-clause
                  :type string
                  :initarg :search-clause))
  (:default-initargs
    :use-optimized nil))

(defmethod export-list-getter ((page search-computer-list))
  (mapcar 
    (lambda (computer)
      (list (name computer)))      
    (unoptimized-list-getter page))
  )

(defmethod unoptimized-list-getter :around ((page search-computer-list))
  (let ((computers (call-next-method)))
    (setf (operating-systems page) (make-hash-table))
    (when computers
      (dolist (os (select 'operating-system :flatp t
                          :where [in [computer-id] (mapcar #'id computers)]))
        (setf (gethash (computer-id os) (operating-systems page)) os)))
    computers))

(defmethod unoptimized-list-getter ((page search-computer-list))
  ;; Strangely enough, I've found performance is actually better when we do
  ;;  multiple queries and merge the results ourselves, instead of mashing it all
  ;;  into one sql query.
  (with-db
    (macrolet ((sql-object-search (obj &rest props)
                 `(select 'computer :refresh t :distinct t :flatp t
                          :where [and
                          filter-sql
                          [= [slot-value 'computer 'company-id] (company-id user)]
                          [= [slot-value ,obj 'computer-id] [slot-value 'computer 'id]]
                          [or
                          ,@(mapcar
                              (lambda (prop) `[like [slot-value ,obj ,prop] search-clause])
                              props)
                          ]])))
      (let* ((user (user page))
              (filter-sql (get-filter-sql page))
              (search-clause (format nil "%~a%" (search-clause page))))
        (sort
          (remove-duplicates
            (concatenate
              'list
              (select 'computer :refresh t :distinct t  :flatp t
                      :where [and
                      filter-sql
                      [= [slot-value 'computer 'company-id] (company-id user)]
                      [or
                      [like [slot-value 'computer 'alias] search-clause]
                      [like [slot-value 'computer 'note] search-clause]
                      ]])
              (select 'computer :refresh t :distinct t :flatp t
                      :where [and
                      filter-sql
                      [= [slot-value 'computer 'company-id] (company-id user)]
                      [= [slot-value 'software-computer-link 'computer-id] [slot-value 'computer 'id]]
                      [= [slot-value 'software 'id] [slot-value 'software-computer-link 'software-id]]
                      [like [slot-value 'software 'name] search-clause]])
              (sql-object-search 'ip-address 'name)
              (sql-object-search 'cd-rom 'name)
              (sql-object-search 'bios 'name 'serial-number)
             (sql-object-search 'bios 'name 'serial-number)
              (sql-object-search 'hard-drive 'name)
              (sql-object-search 'hot-fix 'description 'name)
              (sql-object-search 'memory 'form-factor)
              (sql-object-search 'network-card 'name 'manufacturer 'mac-address)
              (sql-object-search 'operating-system 'name 'version)
              (sql-object-search 'printer 'name)
              (sql-object-search 'processor 'name)
              (sql-object-search 'sound-device 'name 'manufacturer)
              ;(sql-object-search 'startup 'name)
              (sql-object-search 'motherboard 'name 'manufacturer 'serial-number)
              (sql-object-search 'user-account 'name)
              (sql-object-search 'video-controller 'name 'driver))
            :test #'name=)
          #'name<)))))


#|(marshall-to-computers
                            (query (format nil "select distinct computers.ID, computers.COMPANY_ID, computers.NAME,
computers.ARCHON_CONNECTION, computers.NOTE, computers.ONLINE, computers.LAST_ONLINE, computers.WARRANTY 
from computers
 left outer join ip_address on ip_address.COMPUTER_ID = computers.ID
 left outer join cd_roms on cd_roms.COMPUTER_ID = computers.ID
 left outer join bios on bios.COMPUTER_ID = computers.ID
 left outer join hard_drives on hard_drives.COMPUTER_ID = computers.ID
 left outer join hotfixes on hotfixes.COMPUTER_ID = computers.ID
 left outer join network_cards on network_cards.COMPUTER_ID = computers.ID
 left outer join operating_systems on operating_systems.COMPUTER_ID = computers.ID
 left outer join printers on printers.COMPUTER_ID = computers.ID
 left outer join processors on processors.COMPUTER_ID = computers.ID
 left outer join sound_devices on sound_devices.COMPUTER_ID = computers.ID
 left outer join motherboards on motherboards.COMPUTER_ID = computers.ID
 left outer join user_accounts on user_accounts.COMPUTER_ID = computers.ID
 left outer join video_controllers on video_controllers.COMPUTER_ID = computers.ID
where
 ip_address.NAME like ~@*~a or
 cd_roms.NAME like ~@*~a or
 bios.NAME like ~@*~a or
 hard_drives.NAME like ~@*~a or
 hotfixes.DESCRIPTION like ~@*~a or
 network_cards.NAME like ~@*~a or
 operating_systems.NAME like ~@*~a or
 printers.NAME like ~@*~a or
 processors.NAME like ~@*~a or
 sound_devices.NAME like ~@*~a or
 printers.NAME like ~@*~a or
 processors.NAME like ~@*~a or
 sound_devices.NAME like ~@*~a or
 motherboards.NAME like ~@*~a or
 user_accounts.NAME like ~@*~a or
 video_controllers.NAME like ~@*~a
" search-clause))))|#      


#.(clsql:restore-sql-reader-syntax-state)

