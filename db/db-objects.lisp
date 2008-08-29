#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

;;; Contains nearly all of the database/object mappings


(in-package :db)

#.(clsql:locally-enable-sql-reader-syntax)

(defgeneric equiv (obj1 obj2)
  (:documentation "Compares two db objects and tells if they are equivalent.
Only compares properties that should remain constant.
Exists for the purposes of deciding if we need to log an event."))

(defgeneric strict-equiv (obj1 obj2)
  (:documentation "Compares two db objects and tells if they are completely equivalent.
Compares all slots. Exists to determine if we need to update the object in the database."))

(defmacro defequiv (class properties)
  "Generates the equiv method for the given class on the given slots"
  `(defmethod equiv ((obj1 ,class) (obj2 ,class))
     (and ,@(mapcar
              (lambda (property)
                (if (listp property)
                    (let ((accessor (car property))
                          (type (second property))
                          (tmp1 (gensym))
                          (tmp2 (gensym)))
                      (case type
                            (:string
                              `(let ((,tmp1 (,accessor obj1))
                                     (,tmp2 (,accessor obj2)))
                                 (equal (if (stringp ,tmp1)
                                            (string-right-trim '(#\Space #\Tab #\Newline) ,tmp1)
                                            ,tmp1)
                                        (if (stringp ,tmp2)
                                            (string-right-trim '(#\Space #\Tab #\Newline) ,tmp2)
                                            ,tmp2))))
                            (otherwise
                              (error "unknown type given to defequiv"))))
                    `(equal (,property obj1) (,property obj2))))
              properties))))

(defmacro defstrict-equiv (class properties)
  "Generates the strict-equiv method for the class"
  `(defmethod strict-equiv ((obj1 ,class) (obj2 ,class))
     (and ,@(mapcar
              (lambda (property)
                (if (listp property)
                    (let ((accessor (car property))
                          (type (second property))
                          (tmp1 (gensym))
                          (tmp2 (gensym)))
                      (case type
                            (:string
                              `(let ((,tmp1 (,accessor obj1))
                                     (,tmp2 (,accessor obj2)))
                                 (equal (if (stringp ,tmp1)
                                            (string-right-trim '(#\Space #\Tab #\Newline) ,tmp1)
                                            ,tmp1)
                                        (if (stringp ,tmp2)
                                            (string-right-trim '(#\Space #\Tab #\Newline) ,tmp2)
                                            ,tmp2))))
                            (otherwise
                              (error "unknown type given to defstrict-equiv"))))
                    `(equal (,property obj1) (,property obj2))))
              properties))))

(def-view-class db-obj ()
  ((id :db-kind :key
       :accessor id
       :db-constraints (:not-null :auto-increment)
       :type integer
       :initarg :id))
  (:documentation "The base class for all of our database objects, because everybody needs an id"))


(def-view-class system-note (db-obj)
  ((timestamp :accessor timestamp
               :type wall-time
               :initarg :last-login
               :initform (clsql:get-time))
   (note :accessor note
         :type string
         :initarg :note
         :initform ""))
  (:base-table system-notes))

(def-view-class templar-update (db-obj)
  ((begin :accessor begin
	  :type list
	  :initarg :begin
	  :initform ())
   (end :accessor end
	:type list
	:initarg :end
	:initform ())
   (files :accessor files
         :type list
         :initarg :files
         :initform ()))
  (:base-table templar-updates))



(def-view-class company (db-obj)
  ((name :accessor name 
	 :db-constraints :not-null 
	 :type string 
	 :initarg :name) 
   (secret :accessor secret
	   :db-constraints :not-null 
	   :type string
	   :initarg :secret) 
   (msi :accessor msi
        :type string
        :initarg :msi
        :initform nil)
   (level :accessor level
          :type integer
          :initarg :level
          :initform 0)
   (created :accessor created
            :type date
            :initarg :created
            :initform (get-date))
   (disabled :accessor disabled
             :type boolean
             :initarg :disabled
             :initform nil)
   (users :accessor users
	  :db-kind :join
	  :db-info (:join-class user
				:home-key id
				:foreign-key company-id
				:set t))
   (alerts :accessor alerts
           :db-kind :join
           :db-info (:join-class alert
                      :home-key id
                      :foreign-key company-id
                      :set t))
   (groups :accessor groups
           :db-kind :join
           :db-info (:join-class group
                      :home-key id
                      :foreign-key company-id
                      :set t))
   (computers :accessor computers
	      :db-kind :join
	      :db-info (:join-class computer
				    :home-key id
				    :foreign-key company-id
				    :set t))
   (clients :accessor clients
	      :db-kind :join
	      :db-info (:join-class client
				    :home-key id
				    :foreign-key client-id
				    :set t))
   (software :accessor software
             :db-kind :join
             :db-info (:join-class software
                       :home-key id
                       :foreign-key company-id
                       :set t)))
  (:base-table companies))

(defconstant +account-free+ 0)
(defconstant +account-basic+ 1)
(defconstant +account-plus+ 2)
(defconstant +account-premium+ 3)

(defun =account-free (company)
  (equal (level company) +account-free+))

(defun >=account-basic (company)
  (let ((level (level company)))
    (or
      (trial-enabled-p company)
      (and level (>= level +account-basic+)))))

(defun >=account-plus (company)
  "Has access to the ticketing system and alerts"
  (let ((level (level company))) 
    (or
      (trial-enabled-p company)
      (and level (>= level +account-plus+)))))

(defun >=account-premium (company)
  "Has remote capabilities"
  (let ((level (level company)))
    (or
      (trial-enabled-p company)
      (and level (>= level +account-premium+)))))
  
(defun trial-enabled-p (company)
  (let ((date (created company))
        (earlier (date- (get-date) (make-duration :day 30))))
    (and date (date>= date earlier))))

(def-view-class company-property (db-obj)
  ((company-id :accessor company-id
                :db-constraints :not-null
                :type integer
                :initarg :company-id)
   (company :accessor company
            :initarg :company
            :db-kind :join
            :db-info (:join-class company
                      :home-key company-id
                      :foreign-key id
                      :set nil)))
  (:documentation "Base class for objects that are linked to a company"))


;;;; WARNING!!!!!!
;;;; YOU ARE NOT DONE ONCE YOU ADD YOUR SLOT HERE!!!!!
;;;; There is a cache in nexus that grabs user information. You need to add
;;;; the slot information to that function. it is in nexus/functions.lisp.

(def-view-class user (company-property)
  ((level :accessor level
          :type integer
          :db-constraints (:not-null)
          :initarg :level
          :initform 0)
   (name :accessor name
         :type (string 100)
         :initarg :name
         :initform "")
   (email :accessor email
          :type (string 50)
          :initarg :email
          :initform "")
   (username :accessor username
             :db-constraints :not-null
             :type (string 50)
             :initarg :username
             :initform "")
   (password :accessor password
             :db-constraints :not-null
             :type (string 100)
             :initarg :password
             :initform "")
   (last-login :accessor last-login
               :type wall-time
               :initarg :last-login
               :initform (clsql:get-time))
   (logged-last-login :accessor logged-last-login
                      :type wall-time
                      :db-kind :virtual)
   (weekly-software-report :accessor weekly-software-report
                           :type boolean
                           :initarg :weekly-software-report)
   (timezone-preference :accessor timezone-preference
			:type integer
			:initarg :timezone-preference
				 :initform 0)
   (recent-computers :accessor recent-computers
		     :type list
		     :initarg :recent-computers
		     :initform ()
                     :documentation "Purer database design would probably put this in a separate table,
but we need it on every page, so for efficiency's sake, we put it here.")
   (recent-tickets :accessor recent-tickets
                   :db-kind :join
                   :db-info (:join-class recent-ticket
                              :home-key id
                              :foreign-key user-id
                              :set t)))
  (:base-table users))


(def-view-class user-property (db-obj)
  ((user-id :accessor user-id
	    :type integer
	    :initarg :user-id)
   (user :accessor user
	 :db-kind :join
	 :db-info (:join-class user
			       :home-key user-id
			       :foreign-key id
			       :set nil)))
  (:documentation "Base class for anything that needs to link to a user"))

(def-view-class user-session (user-property)
  ((session-id :accessor session-id
               :db-constraints :not-null
               :type (string 100)
               :initarg :session-id)
   (expiration :accessor expiration
               :db-constraints :not-null
               :type wall-time
               :initform (time+ (get-time) (make-duration :day 14))
               :initarg :expiration))
  (:documentation "We create one of these for any login session.
This allows us to save 'remember me' info on the browser without putting
 anything risky (like the password) in their cookies.")
  (:base-table user-sessions))

(defmethod computers ((user user))
  (select 'computer
          :order-by [name]
          :where [= [company-id] (company-id user)]
          :refresh t))


(def-view-class logged-error (company-property)
  ((body :accessor body
         :initarg :body
         :type string)
   (timestamp :accessor timestamp
              :initarg :timestamp
              :initform (get-time)
              :type wall-time))
  (:documentation "If we have an error we want to display to the user, this is a good place to keep it.")
  (:base-table logged-errors))

(def-view-class client (company-property)
  ((name :accessor name
	 :db-constraints :not-null
	 :type string
	 :initarg :name
	 :initform ""))
  (:base-table clients))

(def-view-class computer (company-property)
  ((archon-connection :accessor archon-connection
	       :type integer
	       :initarg :archon-connection
	       :initform 0)
   (client-id :accessor client-id
	      :type integer
	      :initarg :client-id)
   (client  :accessor client
	    :initarg :client
		     :db-kind :join
		     :db-info (:join-class client
			       :home-key client-id
			       :foreign-key id
			       :set nil))
   (name :accessor name
         :db-constraints :not-null
         :type (string 75)
         :initarg :name)
   (alias :accessor alias
          :db-constraints :not-null
          :type (string 75)
          :initarg :name)
   (note :accessor note
         :type string
         :initarg :note
         :initform "")
   (online :accessor online
           :type boolean
           :initarg :online
           :initform nil)
   (last-online :accessor last-online
                :type wall-time
                :initarg :last-online)
   (warranty :accessor warranty
             :type date
             :initform nil)
   (tags :accessor tags
         :db-kind :join
         :db-info (:join-class computer-tag
                   :home-key id
                   :foreign-key computer-id
                   :set t))
   (bios :accessor bios
         :db-kind :join
         :db-info (:join-class bios
                   :home-key id
                   :foreign-key computer-id
                   :set t))
   (services :accessor services
	     :db-kind :join
	     :db-info (:join-class service
				   :home-key id
				   :foreign-key computer-id
				   :set t))
   (cd-roms :accessor cd-roms
            :db-kind :join
            :db-info (:join-class cd-rom
                      :home-key id
                      :foreign-key computer-id
                      :set t))
   (hard-drives :accessor hard-drives
                :db-kind :join
                :db-info (:join-class hard-drive
                          :home-key id
                          :foreign-key computer-id
                          :set t))
   (logical-drives :accessor logical-drives
                :db-kind :join
                :db-info (:join-class logical-drive
                          :home-key id
                          :foreign-key computer-id
                          :set t))
   (hardware-errors :accessor hardware-errors
                    :db-kind :join
                    :db-info (:join-class hardware-error
                              :home-key id
                              :foreign-key computer-id
                              :set t))
   (hot-fixes :accessor hot-fixes
              :db-kind :join
              :db-info (:join-class hot-fix
                        :home-key id
                        :foreign-key computer-id
                        :set t))
   (ip-addresses :accessor ip-addresses
                 :db-kind :join
                 :db-info (:join-class ip-address
                           :home-key id
                           :foreign-key computer-id
                           :set t))
   (memory :accessor memory
         :db-kind :join
         :db-info (:join-class memory
                   :home-key id
                   :foreign-key computer-id
                   :set t))
   (memory-array :accessor memory-array
		 :db-kind :join
		 :db-info (:join-class memory-array
				       :home-key id
				       :foreign-key computer-id
				       :set t))
   (network-cards :accessor network-cards
                  :db-kind :join
                  :db-info (:join-class network-card
                            :home-key id
                            :foreign-key computer-id
                            :set t))
   (operating-system :accessor operating-system
                     :db-kind :join
                     :db-info (:join-class operating-system
                               :home-key id
                               :foreign-key computer-id
                               :set t))
   (antivirus :accessor antivirus
                     :db-kind :join
                     :db-info (:join-class antivirus
                               :home-key id
                               :foreign-key computer-id
                               :set t))
   (firewall :accessor firewall
                     :db-kind :join
                     :db-info (:join-class firewall
                               :home-key id
                               :foreign-key computer-id
                               :set t))
   (printers :accessor printers
             :db-kind :join
             :db-info (:join-class printer
                       :home-key id
                       :foreign-key computer-id
                       :set t))
   (processes :accessor processes
              :db-kind :virtual)
   (processors :accessor processors
               :db-kind :join
               :db-info (:join-class processor
                         :home-key id
                         :foreign-key computer-id
                         :set t))
   (software :accessor software
             :db-kind :join
             :db-info (:join-class software-computer-link
                       :home-key id
                       :foreign-key computer-id
                       :set t))
   (sound-devices :accessor sound-devices
                  :db-kind :join
                  :db-info (:join-class sound-device
                            :home-key id
                            :foreign-key computer-id
                            :set t))
   (startups :accessor startups
             :db-kind :join
             :db-info (:join-class startup
                       :home-key id
                       :foreign-key computer-id
                       :set t))
   (motherboards :accessor motherboards
                 :db-kind :join
                 :db-info (:join-class motherboard
                           :home-key id
                           :foreign-key computer-id
                           :set t))
   (user-accounts :accessor user-accounts
                  :db-kind :join
                  :db-info (:join-class user-account
                            :home-key id
                            :foreign-key computer-id
                            :set t))
   (video-controllers :accessor video-controllers
                      :db-kind :join
                      :db-info (:join-class video-controller
                                :home-key id
                                :foreign-key computer-id
                                :set t))
   (events :accessor events
	   :db-kind :join
	   :db-info (:join-class event
				 :home-key id
				 :foreign-key computer-id
				 :set t))
   (alerts 
           :db-kind :join
           :db-info (:join-class alert-computer-link
                     :foreign-key computer-id
                     :home-key id
                     :set t))
   )
  (:base-table computers))



(def-view-class computer-property (db-obj)
  ((computer-id :accessor computer-id
                :db-constraints :not-null
                :type integer
                :initarg :computer-id)
   (computer  :accessor computer
              :initarg :computer
              :db-kind :join
              :db-info (:join-class computer
                        :home-key computer-id
                        :foreign-key id
                        :set nil)))
  (:documentation "Base class for things which link to a computer, which many things do"))





(def-view-class service (computer-property)
  ((name :accessor name
	 :initarg :name
	 :type string)
   (description :accessor description
		:initarg :description
		:type string)))

(defequiv service ((name :string)))
(defstrict-equiv service ((name :string)))

(def-view-class computer-tag (computer-property)
  ((name :accessor name
         :db-constraints :not-null
         :type (string 50)
         :initarg :name)))


(def-view-class event-type (db-obj)
  ((name :accessor name
         :db-constraints :not-null
         :type (string 20)
         :initarg :name))
  (:base-table event-types))
    
(defconstant +event-online+ 0)
(defconstant +event-offline+ 1)
(defconstant +event-hardware-add+ 2)
(defconstant +event-hardware-remove+ 3)
(defconstant +event-software-add+ 4)
(defconstant +event-software-remove+ 5)
(defconstant +event-hotfix-add+ 6)
(defconstant +event-hotfix-remove+ 7)
(defconstant +event-alert+ 8)
(defconstant +event-service-add+ 9)
(defconstant +event-service-remove+ 10)
(defconstant +event-user-add+ 11)
(defconstant +event-user-remove+ 12)
(defconstant +event-hardware-error-add+ 13)
(defconstant +event-hardware-error-remove+ 14)
(defconstant +event-remote-desktop+ 15)


(def-view-class event (computer-property company-property)
  ((summary :accessor summary
            :type string
            :initarg :summary)
   (description :accessor description
                :type string
                :initarg :description
                :initform "")
   (timestamp :accessor timestamp
              :type wall-time
              :initarg :timestamp
              :initform (get-time))
   (severity-id :accessor severity-id
                :type integer
                :initarg :severity-id
                :initform 0)
   (type-id :accessor type-id
            :type integer
            :initarg :type-id)
   (note :accessor note
         :type string
         :initarg :note
         :initform "")
   (severity :accessor severity
             :initarg :type
             :db-kind :join
             :db-info (:join-class event-type
                       :home-key severity-id
                       :foreign-key id
                       :set nil)))
  (:documentation "Pure db design would probably leave out the company-id since we can
 technically get it from the computer we link to, but in reality we accumulate so many events that
 it ends up gong far too slowly without it.")
  (:base-table events))


(def-view-class software-event (computer-property)
  ((name :accessor name
         :db-constraints :not-null
         :type string
         :initarg :name)
   (timestamp :accessor timestamp
              :type wall-time
              :initarg :timestamp)
   (installed :accessor installed
              :type boolean
              :initarg :installed
              :documentation "t if this property was installed, nil if uninstalled")))



(def-view-class operating-system (computer-property)
  ((name :accessor name
         :db-constraints :not-null
         :type (string 50)
         :initarg :name
         :initform "")
   (version :accessor version
            :type (string 50)
            :initarg :version
            :initform "")
   (product-id :accessor product-id
               :type (string 200)
               :initarg :product-id
               :initform "")
   (license-key :accessor license-key
                :type (string 200)
                :initarg :license-key
                :initform "")
   (registered-user :accessor registered-user
                    :type (string 50)
                    :initarg :registered-user
                    :initform "")
   (service-pack :accessor service-pack
                 :type (string 100)
                 :initarg :service-pack
                 :initform ""))
  (:base-table operating_systems))

(defstrict-equiv operating-system ((name :string) (version :string) (product-id :string) 
				   (registered-user :string) (service-pack :string)))

(defequiv operating-system ((name :string) (version :string) (product-id :string)))

(def-view-class antivirus (computer-property)
  ((name :accessor name
         :type (string 200)
         :initarg :name
         :initform "")
   (manufacturer :accessor manufacturer
            :type (string 200)
            :initarg :manufacturer
            :initform "")
   (company :accessor company
               :type (string 200)
               :initarg :company
               :initform "")
   (version :accessor version
                    :type (string 50)
                    :initarg :version
                    :initform "")
   (up-to-date :accessor up-to-date
                 :type boolean
                 :initarg :up-to-date
                 :initform ""))
  (:base-table antivirus))

(defstrict-equiv antivirus ((name :string) (manufacturer :string) (company :string) 
				   (version :string) up-to-date))

(defequiv antivirus ((name :string) (manufacturer :string) (company :string) 
				   (version :string) up-to-date))

(def-view-class firewall (computer-property)
  ((name :accessor name
         :type (string 200)
         :initarg :name
         :initform "")
   (company :accessor company
            :type (string 200)
            :initarg :company
            :initform "")
   (version :accessor version
                    :type (string 50)
                    :initarg :version
                    :initform "")
   (enabled :accessor enabled
                 :type boolean
                 :initarg :enabled
                 :initform ""))
  (:base-table firewall))

(defstrict-equiv firewall ((name :string) (company :string) 
				   (version :string) enabled))

(defequiv firewall ((name :string) (manufacturer :string) (company :string) 
				   (version :string) enabled))

(def-view-class processor (computer-property)
  ((name :accessor name
         :type (string 100)
         :initarg :name)
   (info :accessor info
         :type (string 200)
         :initarg :info)
   (architecture :accessor architecture
                 :type (string 100)
                 :initarg :architecture)
   (l2-cache :accessor l2-cache
             :type integer
             :initarg :l2-cache)
   (clock-speed :accessor clock-speed
                :type integer
                :initarg :clock-speed))
  (:base-table processors))

(defequiv processor ((name :string) (info :string) (architecture :string)))
(defstrict-equiv processor ((name :string) (info :string) (architecture :string) l2-cache clock-speed))


(def-view-class memory (computer-property)
  ((capacity :accessor capacity
	     :type integer
	     :initarg :capacity)
   (speed :accessor memory-speed
          :type integer
          :initarg :memory-speed)
   (form-factor :accessor form-factor
                :type (string 100)
                :initarg :form-factor)
   (location :accessor location
                :type (string 100)
                :initarg :location)
   (manufacturer :accessor manufacturer
                :type (string 100)
                :initarg :manufacturer))
  (:base-table memory))

(defequiv memory (capacity memory-speed (form-factor :string) (manufacturer :string)))
(defstrict-equiv memory (capacity memory-speed (form-factor :string) (location :string) (manufacturer :string)))

(def-view-class memory-array (computer-property)
  ((max-capacity :accessor max-capacity
		 :type integer
		 :initarg :max-capacity)
   (num-slots :accessor num-slots
                :type integer
                :initarg :num-slots)
   (error-correction :accessor error-correction
		     :type integer
		     :initarg :error-correction))
  (:base-table memory-arrays))

(defequiv memory-array (max-capacity num-slots error-correction))
(defstrict-equiv memory-array (max-capacity num-slots error-correction))

(defconstant +memory-error-correction-reserved+ 0)
(defconstant +memory-error-correction-other+ 1)
(defconstant +memory-error-correction-unknown+ 2)
(defconstant +memory-error-correction-none+ 3)
(defconstant +memory-error-correction-parity+ 4)
(defconstant +memory-error-correction-single-ecc+ 5)
(defconstant +memory-error-correction-multi-ecc+ 6)
(defconstant +memory-error-correction-crc+ 7)

(defun error-correction-string (err)
  (case err
    (#.+memory-error-correction-reserved+ "Reserved")
    (#.+memory-error-correction-other+ "Other")
    (#.+memory-error-correction-unknown+ "Unknown")
    (#.+memory-error-correction-none+ "None")
    (#.+memory-error-correction-parity+ "Parity")
    (#.+memory-error-correction-single-ecc+ "Single-bit ECC")
    (#.+memory-error-correction-multi-ecc+ "Multiple-bit ECC")
    (#.+memory-error-correction-crc+ "CRC")))

(def-view-class bios (computer-property)
  ((name :accessor name
         :type (string 100)
         :initarg :name
         :initform "")
   (language :accessor language
             :type (string 100)
             :initarg :language
             :initform "")
   (manufacturer :accessor manufacturer
                 :type (string 100)
                 :initarg :manufacturer
                 :initform "")
   (release-date :accessor release-date
                 :type date
                 :initarg :release-date)
   (version :accessor version
            :type (string 100)
            :initarg :version
            :initform "")
   (serial-number :accessor serial-number
                  :type (string 100)
                  :initarg :serial-number
                  :initform ""))
  (:base-table bios))

(defequiv bios ((name :string) (language :string) (manufacturer :string)
                (version :string) (serial-number :string)))
(defstrict-equiv bios ((name :string) (language :string) (manufacturer :string)
                       (version :string) (serial-number :string)))


(def-view-class cd-rom (computer-property)
  ((name :accessor name
         :type (string 100)
         :initarg :name
         :initform "")
   (drive :accessor drive
          :type (string 100)
          :initarg :drive
          :initform "")
   (manufacturer :accessor manufacturer
                 :type (string 100)
                 :initarg :manufacturer
                 :initform ""))
  (:base-table cd_roms))

(defequiv cd-rom ((name :string) (drive :string) (manufacturer :string)))
(defstrict-equiv cd-rom ((name :string) (drive :string) (manufacturer :string)))


(def-view-class hard-drive (computer-property)
  ((name :accessor name
         :type (string 100)
         :initarg :name)
   (size :accessor size
         :type bigint
         :initarg :size)
   (manufacturer :accessor manufacturer
         :type (string 100)
         :initarg :manufacturer)
   (interface-type :accessor interface-type
                   :type (string 100)
                   :initarg :interface-type))
  (:base-table hard_drives))

(defequiv hard-drive ((name :string) size (interface-type :string)))
(defstrict-equiv hard-drive ((name :string) size (manufacturer :string) (interface-type :string)))

(def-view-class logical-drive (computer-property)
  ((name :accessor name
         :type (string 100)
         :initarg :name)
   (size :accessor size
         :type bigint
         :initarg :size)
   (free-space :accessor free-space
         :type bigint
         :initarg :free-space))
  (:base-table logical_drives))

(defequiv logical-drive ((name :string) size))
(defstrict-equiv logical-drive ((name :string) size free-space))

(def-view-class ip-address (computer-property)
  ((name :accessor name
         :type (string 100)
         :initarg :name)))

(def-view-class video-controller (computer-property)
  ((name :accessor name
         :type (string 100)
         :initarg :name)
   (horizontal-resolution :accessor horizontal-resolution
                          :type integer
                          :initarg :horizontal-resolution)
   (vertical-resolution :accessor vertical-resolution
                        :type integer
                        :initarg :vertical-resolution)
   (refresh-rate :accessor refresh-rate
                 :type integer
                 :initarg :refresh-rate)
   (driver :accessor driver
           :type (string 100)
           :initarg :driver))
  (:base-table video_controllers))

(defequiv video-controller ((name :string)))
(defstrict-equiv video-controller ((name :string) horizontal-resolution vertical-resolution refresh-rate
                                   (driver :string)))

(def-view-class sound-device (computer-property)
  ((name :accessor name
         :type (string 100)
         :initarg :name)
   (manufacturer :accessor manufacturer
                 :type (string 100)
                 :initarg :manufacturer))
  (:base-table sound_devices))

(defequiv sound-device ((name :string) (manufacturer :string)))
(defstrict-equiv sound-device ((name :string) (manufacturer :string)))

(def-view-class network-card (computer-property)
  ((name :accessor name
         :type (string 100)
         :initarg :name)
   (mac-address :accessor mac-address
                :type (string 100)
                :initarg :mac-address)
   (manufacturer :accessor manufacturer
                 :type (string 100)
                 :initarg :manufacturer))
  (:base-table network-cards))

(defequiv network-card ((name :string) (mac-address :string) (manufacturer :string)))
(defstrict-equiv network-card ((name :string) (mac-address :string) (manufacturer :string)))


(def-view-class motherboard (computer-property)
  ((name :accessor name
         :type (string 100)
         :initarg :name)
   (manufacturer :accessor manufacturer
                 :type (string 100)
                 :initarg :manufacturer)
   (serial-number :accessor serial-number
                  :type (string 100)
                  :initarg :serial-number))
  (:base-table motherboards))

(defequiv motherboard ((serial-number :string) (name :string) (manufacturer :string)))
(defstrict-equiv motherboard ((serial-number :string) (name :string) (manufacturer :string)))


(def-view-class printer (computer-property)
  ((name :accessor name
         :type (string 100)
         :initarg :name)
   (is-default :accessor is-default
               :type boolean
               :initarg :is-default))
  (:base-table printers))

(defequiv printer ((name :string)))
(defstrict-equiv printer ((name :string) is-default))

(def-view-class user-account (computer-property)
  ((name :accessor name
         :type (string 60)
         :initarg :name)
   (locked :accessor locked
           :type boolean
           :initarg :locked)
   (disabled :accessor disabled
             :type boolean
             :initarg :disabled))
  (:base-table user-accounts))

(defequiv user-account ((name :string)))
(defstrict-equiv user-account ((name :string) disabled))

(def-view-class software (company-property)
  ((name :accessor name
         :type (string 100)
         :initarg :name)
   (publisher :accessor publisher
              :type (string 100)
              :initarg :publisher)
   (software-computer-link :accessor software-computer-link
                           :initarg :software
                           :db-kind :join
                           :db-info (:join-class software-computer-link
                                     :home-key id
                                     :foreign-key software-id
                                     :set t)))
  (:documentation "While software could be considered a computer property, we find it
 more useful to view it as belonging to a company, with various copies existing on computers.
This makes it much easier to handle software auditing.")
  (:base-table software))

(defequiv software ((name :string) (publisher :string)))
(defstrict-equiv software ((name :string) (publisher :string)))

(defmethod computers ((obj software))
  (select 'computer :flatp t
          :where [and
          [= [slot-value 'software-computer-link 'software-id] (id obj)]
          [= [slot-value 'computer 'id] [slot-value 'software-computer-link 'computer-id]]]
          :order-by [slot-value 'computer 'name]))

(def-view-class software-property (db-obj)
  ((software-id :accessor software-id
                :db-constraints :not-null
                :type integer
                :initarg :software-id)
   (software :accessor software
             :db-kind :join
             :db-info (:join-class software
                       :home-key software-id
                       :foreign-key id
                       :set nil)))
  (:documentation "Base class for anything that links to software."))

(def-view-class software-computer-link (computer-property software-property)
  ((version :accessor version
            :type (string 100)
            :initarg :version)
   (product-id :accessor product-id
	       :type (string 100)
	       :initarg :product-id)
   (license-key :accessor license-key
		:type (string 100)
		:initarg :license-key)))

(defequiv software-computer-link (software-id computer-id))
(defstrict-equiv software-computer-link (software-id computer-id))

(defmethod name ((obj software-computer-link))
  (name (software obj)))

(defmethod publisher ((obj software-computer-link))
  (publisher (software obj)))

(def-view-class software-key (computer-property)
  ((software :accessor software
             :db-constraints :not-null
             :type (string 100)
             :initarg :software)
   (value :accessor :value
          :db-constraints :not-null
          :type (string 100)
          :initarg :value)))

(defmethod software-key ((computer computer) (software software))
  (car (select 'software-key
               :refresh t :flatp t :limit 1
               :where [and [= [computer-id] (id computer)]
               [= [name] (name software)]])))

(def-view-class license-scheme (company-property)
  ((name :accessor name
         :db-constraints :not-null
         :type (string 100)
         :initarg :name)
   (license-counts :accessor license-counts
                   :db-kind :join
                   :db-info (:join-class license-count
                             :home-key id
                             :foreign-key license-scheme-id
                             :set t)))
  (:documentation "Provides support for a feature that we haven't really implemented yet in the UI.
Basically, this makes it possible for a company to have multiple license schemes.
Handy for IT shops that support multiple companies, each with their own licenses."))


(def-view-class license-count (software-property)
  ((num :accessor num
        :db-constraints :not-null
        :type integer
        :initarg :num)
   (license-scheme-id :accessor license-scheme-id
                      :db-constraints :not-null
                      :type integer
                      :initarg :license-scheme-id)
   (license-scheme :accessor license-scheme
                   :db-kind :join
                   :db-info (:join-class license-scheme
                             :home-key license-scheme-id
                             :foreign-key id
                             :set nil))))

(defmethod license-count ((software software) (scheme license-scheme))
  (car (select 'license-count
                :refresh t :flatp t :limit 1
                :where [and [= [license-scheme-id] (id scheme)]
                [= [software-id] (id software)]])))


(def-view-class startup (computer-property)
  ((name :accessor name
         :type (string 100)
         :initarg :name)
   (location :accessor location
             :type string
             :initarg :location)
   (command :accessor command
            :type string
            :initarg :command)
   (user :accessor user
         :type (string 100)
         :initarg :user))
  (:base-table startups))

(defequiv startup ((name :string) (location :string) (command :string) (user :string)))
(defstrict-equiv startup ((name :string) (location :string) (command :string) (user :string)))

(def-view-class hot-fix (computer-property)
  ((name :accessor name
         :type (string 100)
         :initarg :name)
   (description :accessor description
                :type (string 100)
                :initarg :description)
   (installed-by :accessor installed-by
                 :type (string 100)
                 :initarg :installed-by))
  (:base-table hotfixes))

(defequiv hot-fix ((name :string)))
(defstrict-equiv hot-fix ((name :string) (installed-by :string)))


(def-view-class hardware-error (computer-property)
  ((description :accessor description
                :type string
                :initarg :description))
  (:base-table hardware-errors))

(defequiv hardware-error ((description :string)))
(defstrict-equiv hardware-error ((description :string)))


(defclass process ()
  ((name :accessor name
         :initarg :name
         :initform "")
   (num-threads :accessor num-threads
                :initarg :num-threads
                :initform 1)
   (memory-consumption :accessor memory-consumption
                       :initarg :memory-consumption
                       :initform 0)
   (handles :accessor handles
            :initarg :handles
            :initform 1)
   (id :accessor id
       :initarg :id
       :initform "")
   (computer :accessor computer
             :initarg :computer
             :initform "")))


;;; classes for help desk



(def-view-class company-ticket-id ()
  ((company-id :db-kind :key
	       :accessor company-id
	       :db-constraints :not-null
	       :type integer
	       :initarg :company-id)
   (next-ticket-id :accessor next-ticket-id
	       :type integer
	       :initform 1
	       :initarg :next-ticket-id))
  (:documentation "Each tickets gets a number assigned to it to help identify it.
We can't use the id field, since that would result in large gaps for any system with multiple companies.
So we implement our own autoincrement here.")
  (:base-table company-ticket-ids))

(def-view-class ticket (company-property)
  ((ticket-id :accessor ticket-id
	      :type integer
	      :initarg :ticket-id
              :initform nil)
   (assigned-user-id :accessor assigned-user-id
                     :type integer
                     :initarg :assigned-user-id)
   (state :accessor state
          :db-constraints :not-null
          :type integer
          :initarg :state)
   (priority :accessor priority
             :db-constraints :not-null
             :type integer
             :initarg :priority)
   (timestamp :accessor timestamp
              :type wall-time
              :initform (clsql:get-time)
              :initarg :timestamp)
   (due-date :accessor due-date
             :type wall-time
             :initform nil
             :initarg :due-date)
   (subject :accessor subject
            :type string
            :initarg :subject
            :initform "")
   (body :accessor body
         :type string
         :initarg :body
         :initform "")
   (response-email :accessor response-email
                   :type string
                   :initarg :response-email
                   :initform "")
   (rating :accessor rating
           :type integer
           :initarg :rating)
   (assigned-user :accessor assigned-user
                  :db-kind :join
                  :db-info (:join-class user
                                        :home-key assigned-user-id
                                        :foreign-key id
                                        :set nil))
   (comments :accessor comments
             :db-kind :join
             :db-info (:join-class ticket-comment
                                   :home-key id
                                   :foreign-key ticket-id
                                   :set t))
   (responses :accessor responses
              :db-kind :join
              :db-info (:join-class ticket-response
                                    :home-key id
                                    :foreign-key ticket-id
                                    :set t))
   (changes :accessor changes
            :db-kind :join
            :db-info (:join-class ticket-change
                                  :home-key id
				  :foreign-key ticket-id
				  :set t))
   (tags :accessor tags
         :db-kind :join
         :db-info (:join-class ticket-tag
                   :home-key id
                   :foreign-key ticket-id
                   :set t)))
  (:base-table tickets))

(defconstant +ticket-status-closed+ 0)
(defconstant +ticket-status-open+ 1)
(defconstant +ticket-status-hold+ 2)
(defconstant +ticket-priority-note+ 0)
(defconstant +ticket-priority-low+ 1)
(defconstant +ticket-priority-medium+ 2)
(defconstant +ticket-priority-high+ 3)
(defconstant +ticket-priority-critical+ 4)

(defmethod computers ((ticket ticket))
  (select 'computer :flatp t :order-by [slot-value 'computer 'name]
          :where [and
          [= [slot-value 'computer 'id] [slot-value 'ticket-computer 'computer-id]]
          [= [slot-value 'ticket-computer 'ticket-id] (id ticket)]]))

(defmethod tickets ((computer computer))
  (select 'ticket :flatp t
          :order-by [slot-value 'ticket 'ticket-id]
          :where [and
          [= [slot-value 'ticket-computer 'computer-id] (id computer)]
          [= [slot-value 'ticket-computer 'ticket-id] [slot-value 'ticket 'id]]]))

(defmethod open-tickets ((computer computer))
  (select 'ticket :flatp t
          :order-by [slot-value 'ticket 'ticket-id]
          :where [and
          [= [slot-value 'ticket-computer 'computer-id] (id computer)]
	  [= [slot-value 'ticket 'state] +ticket-status-open+ ]
          [= [slot-value 'ticket-computer 'ticket-id] [slot-value 'ticket 'id]]]))

(defmethod open-tickets-count ((computer computer))
  (car (select [count [*]] :flatp t
               :from '([tickets] [ticket-computers])
               :order-by [slot-value 'ticket 'ticket-id]
               :where [and
               [= [slot-value 'ticket-computer 'computer-id] (id computer)]
               [= [slot-value 'ticket 'state] +ticket-status-open+ ]
               [= [slot-value 'ticket-computer 'ticket-id] [slot-value 'ticket 'id]]])))

(defgeneric adjusted-timestamp (obj user)
  (:method (obj (user user))
    (let ((timestamp (timestamp obj))
	  (offset (timezone-preference user)))
      (when (and timestamp offset)
	(clsql:time+ timestamp (clsql:make-duration :hour offset))))))


(def-view-class ticket-property (db-obj)
  ((ticket-id :accessor ticket-id
              :db-constraints :not-null
              :type integer
              :initarg :ticket-id)
   (ticket :accessor ticket
           :db-kind :join
           :db-info (:join-class ticket
                                 :home-key ticket-id
                                 :foreign-key id
                                 :set nil)))
  (:documentation "Base class for any object that links to tickets."))

(def-view-class recent-ticket (user-property ticket-property)
  ()
  (:documentation "Keeps track of what tickets the user has viewed recently")
  (:base-table recent-tickets))

(def-view-class ticket-tag (ticket-property)
  ((name :accessor name
         :db-constraints :not-null
         :type (string 50)
         :initarg :name))
  (:base-table ticket-tags))

(def-view-class ticket-computer (ticket-property computer-property)
  ()
  (:documentation "Links a ticket to the computers it relates to.")
  (:base-table ticket-computers))

(def-view-class ticket-response (ticket-property)
  ((timestamp :accessor timestamp
               :type wall-time
               :initarg :last-login
               :initform (clsql:get-time))
   (user-id :accessor user-id
	    :type integer
	    :initarg :user-id)
   (sender :accessor sender
           :type string
           :initarg :sender)
   (body :accessor body
	 :type string
	 :initarg :body))
  (:documentation "A response to a ticket will be emailed to the person who submitted it.")
  (:base-table ticket-responses))

(def-view-class ticket-comment (ticket-property user-property)
  ((timestamp :accessor timestamp
               :type wall-time
               :initarg :last-login
               :initform (clsql:get-time))
   (body :accessor body
	 :type string
	 :initarg :body))
  (:documentation "A comment on a ticket will be kept internal")
  (:base-table ticket-comments))

(def-view-class ticket-change (ticket-property user-property)
  ((timestamp :accessor timestamp
               :type wall-time
               :initarg :last-login
               :initform (clsql:get-time))
   (description :accessor description
		:type string
		:initarg :description)
   (note :accessor note
         :type string
         :initarg :note))
  (:documentation "We make sure to keep track of any changes to a ticket.")
  (:base-table ticket-changes))



;;; Groups

(def-view-class group (company-property)
  ((name :accessor name
         :type string
         :initarg :name)
   (all-computers :accessor all-computers
                  :type boolean
                  :initarg :all-computers
                  :initform t)
   (remote-permission :accessor remote-permission
                      :type boolean
                      :initarg :remote-permission
                      :initform t)
   (shutdown-permission :accessor shutdown-permission
                        :type boolean
                        :initarg :shutdown-permission
                        :initform t)
   (note-permission :accessor note-permission
                    :type boolean
                    :initarg :note-permission
                    :initform t))
  (:documentation "Groups exist to control permissions within Nexus")
  (:base-table groups))

(def-view-class group-property (db-obj)
  ((group-id :accessor group-id
             :db-constraints (:not-null)
             :type integer
             :initarg :group-id)
   (group :accessor group
          :db-kind :join
          :db-info (:join-class group
                                :home-key id
                                :foreign-key group-id
                                :set nil)))
  (:documentation "Base class for anything that links to a group."))

(def-view-class group-user-link (group-property user-property)
  ()
  (:documentation "Determines which users get the permissions of this group")
  (:base-table group-user-links))

(def-view-class group-computer-link (group-property computer-property)
  ()
  (:documentation "Determines which computers this group has permission to act on")
  (:base-table group-computer-links))

(defmethod computers ((obj group))
  (when (slot-boundp obj 'id)
    (select 'computer :flatp t
            :where [and
            [= [slot-value 'group-computer-link 'group-id] (id obj)]
            [= [slot-value 'computer 'id] [slot-value 'group-computer-link 'computer-id]]]
            :order-by [slot-value 'computer 'name])))

(defmethod users ((obj group))
  (when (slot-boundp obj 'id)
    (select 'user :flatp t
            :where [and
            [= [slot-value 'group-user-link 'group-id] (id obj)]
            [= [slot-value 'user 'id] [slot-value 'group-user-link 'user-id]]])))

(defmethod groups ((obj user))
  (select 'group :flatp t
          :order-by [slot-value 'group 'name]
          :where [and
          [= [slot-value 'group-user-link 'user-id] (id obj)]
          [= [slot-value 'group 'id] [slot-value 'group-user-link 'group-id]]]))

(defmethod groups ((company-id integer))
  (select 'group :flatp t
          :order-by [slot-value 'group 'name]
          :where [= [slot-value 'group 'company-id] company-id]))

(defmethod groups ((computer computer))
  (select 'group :flatp t
          :order-by [slot-value 'group 'name]
          :where [and
          [= [slot-value 'group-computer-link 'computer-id] (id computer)]
          [= [slot-value 'group-computer-link 'group-id] [slot-value 'group 'id]]]))



;;; holds account info for the email we watch for tickets
(def-view-class email-smtp (company-property)
  ((host :accessor host
         :initarg :host
         :initform ""
         :type string)
   (port :accessor port
         :initarg :port
         :initform 25
         :type integer)
   (sslp :accessor sslp
	 :initarg :sslp
	 :initform nil
	 :type boolean)
   (reply-to :accessor reply-to
	 :initarg :reply-to
	 :initform ""
	 :type string)
   (username :accessor username
             :initarg :username
             :initform ""
             :type string)
   (password :accessor password
             :initarg :password
             :initform ""
             :type string)
   (authp :accessor authp
	  :initarg :authp
	  :initform t
	  :type boolean))
  (:base-table email-smtp))


(defmethod email-smtp ((company-id integer))
  (or (car (select 'email-smtp :flatp t 
			       :refresh t
			       :where [= [company-id] company-id]))
      (make-instance 'email-smtp :company-id company-id)))
    

(def-view-class ticket-email (company-property)
  ((host :accessor host
         :initarg :host
         :initform ""
         :type string)
   (port :accessor port
         :initarg :port
         :initform 110
         :type integer)
   (username :accessor username
             :initarg :username
             :initform ""
             :type string)
   (password :accessor password
             :initarg :password
             :initform ""
             :type string))
  (:base-table ticket-emails))


(defmethod ticket-emails ((company-id integer))
  (select 'ticket-email :flatp t :where [= [company-id] company-id]))

(defconstant +discount-code-percent+ 0)
(defconstant +discount-code-fixed+ 1)

(def-view-class discount-code (db-obj)
  ((code :accessor code
	 :initarg :code
	 :type string)
   (amount :accessor amount
	   :initarg :amount
	   :type integer)
   (expiration :accessor expiration
	       :initarg :expiration
	       :type date)
   (discount-type :accessor discount-type
		  :initarg :discount-type
		  :initform 0
	          :type integer)
   (duration :accessor duration
	     :initarg :duration
	     :initform 12
	     :type integer))
  (:base-table discount-codes))  

(def-view-class subscription (company-property)
  ((subscription-id :accessor subscription-id
                    :initarg :subscription-id
                    :type (string 20))
   (status :accessor return-status
	   :initarg :return-status
	   :type string)
   (timestamp :accessor timestamp
	      :initarg :timestamp
	      :type wall-time
	      :initform (get-time))
   (first-name :accessor first-name
	       :initarg :first-name
	       :type string)
   (last-name :accessor last-name
	      :initarg :last-name
	      :type string)
   (address :accessor address
	    :initarg :address
	    :type string)
   (city :accessor city
	 :initarg :city
	 :type string)
   (state :accessor state
	  :initarg :state
	  :type string)
   (zip-code :accessor zip-code
	     :initarg :zip-code
	     :type string)
   (phone-number :accessor phone-number
                 :initarg :phone-number
		 :type (string 20))
   (email :accessor email
	  :initarg :email
	  :type string)
   (discount :accessor discount
	     :initarg :discount
	     :type integer)
   (num-computers :accessor num-computers
                  :initarg :num-computers
                  :type integer)
   (level :accessor level
          :initarg :level
          :type integer)
   (commitment :accessor commitment
               :initarg :commitment
               :type integer)
   (amount :accessor amount
	   :initarg :amount
	   :type float))
  (:base-table subscriptions))

(defun =account-paid (company)
  (select 'subscription :flatp t :where [= [company-id] (id company)]))

(defparameter *db-classes*
  '(bios
    cd-rom
    computer
    computer-tag
    alert
    alert-computer-link
    alert-event-link
    hard-drive
    hardware-error
    hot-fix
    memory
    network-card
    operating-system
    printer
    processor
    software
    software-computer-link
    license-scheme
    license-count
    sound-device
    startup
    motherboard
    video-controller
    event
    event-type
    user-account
    user
    company
    system-note
    ip-address
    templar-update
    ticket-computer
    ticket-response
    ticket-comment
    ticket-change
    ticket
    group
    group-computer-link
    group-user-link))

#.(clsql:restore-sql-reader-syntax-state)



