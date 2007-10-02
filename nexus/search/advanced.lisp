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

;;; The following classes are used to define the various searches available in the advanced search

(defcomponent search-field ()
  ((checkbox :accessor checkbox
             :type checkbox-field
             :initarg :checkbox
             :initform (make-instance 'checkbox-field)
             :documentation "Enables this search field")
   (text :accessor text
         :type string
         :initarg :text
         :initform ""
         :documentation "Text displayed to the user describing the search")
   (modifier :accessor modifier
             :initarg :modifier
             :type alist-select-field
             :initform (make-instance 'ucw:alist-select-field
                                     :data-set '(("contains" . :contains) 
						 ("doesn't contain" . :contains-not)
                                                 ("is exactly" . :is) 
						 ("is not" . :is-not)))
             :documentation "Allows the user to choose how they want to search")
   (sql-class :accessor sql-class
              :initarg :sql-class
              :documentation "The db-object that will be searched.")
   (sql-field :accessor sql-field
              :initarg :sql-field
              :documentation "The field of the object to be searched.")
   (sql-table :reader sql-table
              :initarg :sql-table
              :initform nil
              :documentation "The table that the object is found in")
   (clause-field :accessor clause-field
                 :initarg :clause-field
                 :initform (make-instance 'ucw:string-field :css-class "clause")
                 :documentation "This is where the user inputs their search"))
  (:documentation "An advanced search field."))

(defgeneric clear-checkbox (search-field)
  (:documentation "Unselects the search-field"))

(defmethod clear-checkbox ((this search-field))
  ;(setf (value (checkbox this)) nil))
  )

(defgeneric enabled-p (search-field)
  (:documentation "Tells whether the search-field is enabled or not"))

(defmethod enabled-p ((this search-field))
  (and (value (checkbox this)) (value (clause-field this))))

(defmethod initialize-instance :after ((this search-field) &key)
  (push
    `("onblur"
       (if (= (slot-value ($ ,(dom-id (clause-field this))) 'value) "")
           (setf (slot-value ($ ,(dom-id (checkbox this))) 'checked) false)
           (setf (slot-value ($ ,(dom-id (checkbox this))) 'checked) t)))
    (ucw::events (clause-field this)))
  (push
    `("onfocus"
       (setf (slot-value ($ ,(dom-id (checkbox this))) 'checked) t))
    (ucw::events (clause-field this))))

(defmethod render ((page search-field))
  (<:tr
    (<:td
      (render (checkbox page)))
    (<:td
      (<:label :for (dom-id (checkbox page)) (<:ah (text page))))
    (<:td
      (render (modifier page)))
    (<:td
      (render (clause-field page)))))

(defmethod description ((page search-field))
  "Returns a printable description of the search"
  (when (enabled-p page)
    (format nil "~a ~a '~a'"
            (text page)
            (case (value (modifier page))
                  (:contains "contains")
                  (:contains-not "doesn't contain")
                  (:is "is exactly")
                  (:is-not "is not")
                  (:is-greater "is more than")
                  (:is-less "is less than"))
            (value (clause-field page)))))

(defgeneric get-sql-tables (search-field)
  (:documentation "Returns the database tables that need to be searched on"))

(defmethod get-sql-tables ((page search-field))
  (when (enabled-p page)
    (sql-table page)))

(defgeneric as-sql (search-field)
  (:documentation "Returns the sql needed to perform the search"))

(defmethod as-sql ((page search-field))
  (when (enabled-p page)
    (let ((sql-class (sql-class page)))
      [and
      [= [slot-value 'computer 'id] [slot-value sql-class 'computer-id]]
      (case (value (modifier page))
            (:contains [like [slot-value sql-class (sql-field page)]
                 (format nil "%~a%" (value (clause-field page)))])
            (:contains-not
              [not [exists
              [select [*] :from (table-for-object sql-class)
              :where [and [= [slot-value 'computer 'id] [slot-value sql-class 'computer-id]]
              [like [slot-value sql-class 'name] (format nil "%~a%" (value (clause-field page)))]]]]])
            (:is [= [slot-value sql-class (sql-field page)] (value (clause-field page))])
            (:is-not
              [not [exists
              [select [*] :from (table-for-object sql-class)
              :where [and [= [slot-value 'computer 'id] [slot-value sql-class 'computer-id]]
              [= [slot-value sql-class 'name] (value (clause-field page))]]]]])
            (:is-greater [> [slot-value sql-class (sql-field page)]
                         (value (clause-field page))])
            (:is-less [< [slot-value sql-class (sql-field page)]
                      (value (clause-field page))])
            (otherwise (error "unexpected search type")))
      ])))

;;; Search fields for integer values

(defcomponent integer-search-field (search-field)
  ((units :accessor units
          :type string
          :initarg :units
          :initform ""))
  (:documentation "Searches integer fields")
  (:default-initargs
    :modifier (make-instance 'ucw:alist-select-field
                             :data-set '(("is" . :is) 
					 ("is more than" . :is-greater)
                                         ("is less than" . :is-less)))
    :clause-field (make-instance 'ucw:integer-field)))

(defmethod render ((page integer-search-field))
  (<:tr
    (<:td
      (render (checkbox page)))
    (<:td
      (<:label :for (dom-id (checkbox page)) (<:ah (text page))))
    (<:td
      (render (modifier page)))
    (<:td
      (render (clause-field page))
      (<:ah (units page)))))

(defmethod description ((page integer-search-field))
  (when (enabled-p page)
    (format nil "~a ~a" (call-next-method) (units page))))

;;; Custom search fields, for those properties which don't fit the template

(defcomponent memory-search-field (integer-search-field)
  ()
  (:documentation "Searches total memory")
  (:default-initargs
    :units "MB"
    :text " Memory Capacity "
    :sql-table (list [memory])))

(defmethod as-sql ((this memory-search-field))
  (when (enabled-p this)
    (let ((clause (* 1048576 (value (clause-field this))))
          (modifier (value (modifier this))))
      (case modifier
            (:is
              [= clause
              [select [sum [slot-value 'memory 'capacity]] :from [memory]
              :where [= [slot-value 'memory 'computer-id] [slot-value 'computer 'id]]]])
            (:is-not
              [not [= clause
              [select [sum [slot-value 'memory 'capacity]] :from [memory]
              :where [= [slot-value 'memory 'computer-id] [slot-value 'computer 'id]]]]])
            (:is-greater
              [< clause
              [select [sum [slot-value 'memory 'capacity]] :from [memory]
              :where [= [slot-value 'memory 'computer-id] [slot-value 'computer 'id]]]])
            (:is-less
              [> clause
              [select [sum [slot-value 'memory 'capacity]] :from [memory]
              :where [= [slot-value 'memory 'computer-id] [slot-value 'computer 'id]]]])
            (otherwise (error "unexpected search type"))))))

;;; Computer name

(defcomponent computer-name-search-field (search-field)
  ()
  (:default-initargs
    :text " Computer name "))

(defmethod as-sql ((this computer-name-search-field))
  (when (enabled-p this)
    (let ((clause (value (clause-field this)))
          (modifier (value (modifier this))))
      (case modifier
            (:contains
              [like [slot-value 'computer 'name] (format nil "%~a%" clause)])
            (:contains-not
              [not [like [slot-value 'computer 'name] (format nil "%~a%" clause)]])
            (:is
              [= [slot-value 'computer 'name] clause])
            (:is-not
              [not [= [slot-value 'computer 'name] clause]])))))

(defcomponent software-search-field (search-field)
  ()
  (:default-initargs
    :text " Software "
    :sql-table (list [software] [software-computer-link])))

(defmethod get-sql-tables ((this software-search-field))
  (when (enabled-p this)
    (case (value (modifier this))
          ((:contains :is)
           (sql-table this))
          ((:contains-not :is-not)
           nil))))

(defmethod as-sql ((this software-search-field))
  (when (enabled-p this)
    (let ((clause (value (clause-field this)))
          (modifier (value (modifier this))))
      (case modifier
            (:contains
              [and [= [slot-value 'computer 'id] [slot-value 'software-computer-link 'computer-id]]
              [= [slot-value 'software-computer-link 'software-id] [slot-value 'software 'id]]
              [like [slot-value 'software 'name] (format nil "%~a%" clause)]])
            (:contains-not
              (let ((forbidden
                      (with-db
                        (select [slot-value 'software-computer-link 'computer-id]
                                :from (table-for-object 'software-computer-link)
                                :inner-join 'software
                                :on [= [slot-value 'software 'id] [slot-value 'software-computer-link 'software-id]]
                                :where [like [slot-value 'software 'name] (format nil "%~a%" clause)]
                                :flatp t))))
                (if forbidden
                    [not [in [slot-value 'computer 'id] forbidden]]
                    [= 1 1]))
              )
            (:is
              [and [= [slot-value 'computer 'id] [slot-value 'software-computer-link 'computer-id]]
              [= [slot-value 'software-computer-link 'software-id] [slot-value 'software 'id]]
              [= [slot-value 'software 'name] clause]])
            (:is-not
              #|[not [exists 
                   [select [slot-value 'software-computer-link 'id]
                      :from (list [software-computer-link] [software])
                      :where [and 
                         [= [slot-value 'software 'id] [slot-value 'software-computer-link 'software-id]]
                         [= [slot-value 'software 'name] clause]
                         [= [slot-value 'software-computer-link 'computer-id] [slot-value 'computer 'id]]]]]]|#
              (let ((forbidden (with-db
                                 (select [slot-value 'software-computer-link 'computer-id]
                                         :from (table-for-object 'software-computer-link)
                                         :inner-join 'software
                                         :on [= [slot-value 'software 'id] [slot-value 'software-computer-link 'software-id]]
                                         :where [= [slot-value 'software 'name] clause]
                                         :flatp t))))
                (if forbidden
                    [not [in [slot-value 'computer 'id] forbidden]]
                    [= 1 1]))
              )))))

(defcomponent service-search-field (search-field)
  ()
  (:default-initargs
    :text " Service "
    :sql-table nil))


(defmethod as-sql ((this service-search-field))
  (when (enabled-p this)
    (let ((clause (value (clause-field this)))
          (modifier (value (modifier this))))
      (case modifier
            (:contains
              (let ((allowed
                      (with-db
                        (select [slot-value 'service 'computer-id]
                                :flatp t :from (table-for-object 'service)
				:where [like [slot-value 'service 'name] (format nil "%~a%" clause)]))))
                (if allowed
		    [in [slot-value 'computer 'id] allowed]
                    [= 1 1])))
            (:contains-not
              (let ((forbidden
                      (with-db
                        (select [slot-value 'service 'computer-id]
                                :flatp t :from (table-for-object 'service)
				:where [like [slot-value 'service 'name] (format nil "%~a%" clause)]))))
                (if forbidden
		    [not [in [slot-value 'computer 'id] forbidden]]
                    [= 1 1])))
            (:is
	      (let ((allowed
                      (with-db
                        (select [slot-value 'service 'computer-id]
                                :flatp t :from (table-for-object 'service)
				:where [like [slot-value 'service 'name] clause]))))
                (if allowed
		    [in [slot-value 'computer 'id] allowed]
                    [= 1 1])))
            (:is-not
              (let ((forbidden
                      (with-db
                        (select [slot-value 'service 'computer-id]
                                :flatp t :from (table-for-object 'service)
				:where [= [slot-value 'service 'name] clause]))))
                (if forbidden
		    [not [in [slot-value 'computer 'id] forbidden]]
                    [= 1 1])))
              ))))



;;; Finally, the advanced search page itself


(defcomponent advanced-search-page (paragent-window-component)
  ((search-fields :accessor search-fields
                  :initform (init-advanced-search-fields)))
  (:default-initargs
    :title "Advanced Search"))

(defun init-advanced-search-fields ()
  (flet ((make-field (text class field table)
           (make-instance 'search-field :text text :sql-class class :sql-field field :sql-table table)))
    (list
      (make-instance 'computer-name-search-field)
      (make-field " IP Address " 'ip-address 'name (list [ip-address]))
      (make-field " Bios " 'bios 'name (list [bios]))
     (make-field " Bios Serial Number " 'bios 'serial-number (list [bios]))
      (make-field " CD-ROM " 'cd-rom 'name (list [cd-roms]))
      (make-field " Hard Drive " 'hard-drive 'name (list [hard-drives]))
      (make-field " Hardware Error " 'hardware-error 'description (list [hardware-errors]))
      (make-field " Hot fix " 'hot-fix 'name (list [hotfixes]))
      (make-instance 'memory-search-field )
      (make-field " Motherboard " 'motherboard 'name (list [motherboards]))
      (make-field " Network Card " 'network-card 'name (list [network-cards]))
      (make-field " Operating System " 'operating-system 'name (list [operating-systems]))
      (make-field " Processor " 'processor 'name (list [processors]))
      (make-instance 'software-search-field)
      (make-instance 'service-search-field)
      (make-field " Sound Device " 'sound-device 'name (list [sound-devices]))
      (make-field " User Account " 'user-account 'name (list [user-accounts]))
      (make-field " Video Controller " 'video-controller 'name (list [video-controllers]))
  )))

(defgeneric clear-checkboxes (search-page)
  (:documentation "Unselects everybody on the page"))

(defmethod clear-checkboxes ((this advanced-search-page))
  (dolist (search-field (search-fields this))
    (clear-checkbox search-field)))

(defmethod as-sql ((page advanced-search-page))
  (apply
    #'clsql-sys::sql-and
    (cons
      [= [slot-value 'computer 'company-id] (company-id (user page))]
      (remove-if #'null
                 (mapcar
                   #'as-sql
                   (search-fields page))))))



(defmethod get-sql-tables ((page advanced-search-page))
  (cons [computers]
        (mapcan
          ; The following copy-list is essential to avoiding infinite loops, as mapcan is destructive
          (lambda (field) (copy-list (get-sql-tables field)))
          (search-fields page))))
        

(defmethod description ((page advanced-search-page))
  (let ((desc (mapcan
                (lambda (field)
                  (let ((desc (description field)))
                    (if desc (list desc))))
                (search-fields page))))
    (if desc
        (format nil "Computers where ~{~a~^, ~}" desc)
        "All Computers")))
          

(defrender ((page advanced-search-page))
  (<:div
    (show-tip (user page) "This is the Advanced Search page. From here, you can search for computers based on very specific criteria.")
    (<:fieldset
      :class "adv-search"
      (<ucw:form
        :action (do-advanced-search page)
        (<:table
          :width "100%"
          (dolist (search-field (search-fields page))
            ;           (<:ah (as-sql search-field))
            (render search-field)))
        (<:p
          (<ucw:submit :action (do-advanced-search page)
                       :type "image"
                       :class "button"
                       :src "images/searchbtn.gif"))))))

(defaction do-advanced-search ((page advanced-search-page))
  (call-component nil (do-advanced-search% page)))

(defgeneric do-advanced-search% (page)
  (:documentation "Performs the search"))

(defmethod do-advanced-search% ((page advanced-search-page))
  (let* ((user (user page))
         (search-clause (as-sql page))
         (search-page (make-instance 'search-page :user user
                                     :description (description page)
                                     :show-computers nil ;prevent us from initializing the computer-list with useless data
                                     :show-events nil :show-software nil :show-tickets nil))
         (computer-list (make-instance 'computer-list :user user :max-results 20
                                       :place (make-place (computer-list search-page))
                                       :use-optimized t)))
    (setf (computer-list search-page) computer-list)
    (setf (default-view-classes computer-list) (get-sql-tables page))
    (set-filter (computer-list search-page) :search search-clause [computers])
    (setf (show-computers search-page) t)
    (clear-checkboxes page) ; needed to make back work properly
    search-page))


#.(clsql:restore-sql-reader-syntax-state)

