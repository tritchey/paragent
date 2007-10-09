#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#


;;; Define our queries to grab info from the agent
(in-package :archon)

#.(clsql:locally-enable-sql-reader-syntax)

(defgeneric wmi-query (client &rest args))

(defmethod wmi-query ((client client) &rest args)
  (cons 'query args))

(defmacro query-let (bindings client &body body)
  "Makes all these asynchronous calls seem like just another let. Thank you, macros!"
  (labels ((make-send-message (bindings)
           (let ((binding (car bindings)))
             (let ((var (car binding))
                   (command (second binding)))
               `(send-message ,client ,command
                              (lambda (,client &rest ,var)
                                ,(if (cdr bindings)
                                     (make-send-message (cdr bindings))
                                     `(progn
                                        ,@body))))))))
    (make-send-message bindings)))


(defmacro pad-lists (&rest lists)
  `(let ((max-size (apply #'max (mapcar #'length (list ,@lists)))))
    ,@(mapcar
       (lambda (list)
	 `(let* ((len (length ,list))
		 (pad-len (- max-size len)))
	   (setf ,list (append ,list (make-list pad-len :initial-element nil)))))
       lists)))

;(defun profile-set-differences ()
;  (sb-profile:profile set-differences set-difference)
;  (format t "*************************************~%")
;  (dotimes (i 3000)
    ;(my-set-difference *set1* *set2* :test #'equal)
    ;(my-set-difference *set2* *set1* :test #'equal)
;    (set-differences *set1* *set2* :test #'equal)
;    (set-difference *set1* *set2* :test #'equal)
;    (set-difference *set2* *set1* :test #'equal))
;  (sb-profile:report)
;  (sb-profile:unprofile))


(defun my-set-difference (list1 list2 &key (test #'equiv))
  "Return the elements of LIST1 which are not in LIST2."
  (declare (inline member))
  (declare (list list1 list2))
  (if (null list2)
      list1
      (let ((res nil))
        (dolist (elt list1)
          (if (not (member elt list2 :test test))
              (push elt res)))
        res)))

(defun set-differences (list1 list2 &key (test #'equiv))
  "Return the elements of LIST1 which are not in LIST2,
  and the elements of LIST2 which are not in LIST1."
  (declare (optimize (speed 3) (safety 1)))
  (declare (list list1 list2))
  (declare (function test))
  (if (null list2)
      list1
      (let ((list2 (copy-list list2))
            (in-first nil))
        (declare (list list2 in-first))
        (dolist (elt list1)
          (if list2
              (unless (if (funcall test (car list2) elt) ;find and remove the item from list 2
                          (progn
                            (setf list2 (cdr list2))
                            t)
                          (do ((list (cdr list2) (cdr list))
                               (prev list2 (cdr prev)))
                            ((null list))
                            (declare (list list prev))
                            (when (funcall test (car list) elt)
                              (setf (cdr prev) (cdr list))
                              (return t))))
                (push elt in-first))
              (push elt in-first)))
        (values in-first list2))))

(defun lists-match (list1 list2 &key (test #'strict-equiv))
  "If we determine that the lists are identical,
 there's really no need to run a costly set-differences on them."
  (declare (list list1 list2))
  (let ((len1 (length list1))
        (len2 (length list2)))
    (if (equal len1 len2)
        (loop for item1 in list1
              for item2 in list2
              unless (funcall test item1 item2) do (return nil)
              finally (return t))
        nil)))
    

(defmacro defpropquery (name wmi-class wmi-properties db-class db-table db-props
                             &optional added-summary added-description added-props
                             removed-summary removed-description removed-props)
  "Hideously complex macro that makes defining queries hideously easy"
  (let ((prop-lists (loop for n in wmi-properties collect (gensym)))
        (prop-vars (loop for n in wmi-properties collect (gensym)))
        (namespace (if (listp wmi-class) (second wmi-class) nil)))
    (when (listp wmi-class) (setf wmi-class (first wmi-class)))
    `(defgeneric ,name (client)
       (:method ((client client))
         (handler-case
           (query-let
             ,(mapcar
                (lambda (list-name wmi-prop)
                  `(,list-name ,(if namespace `(wmi-query client ,wmi-class ,wmi-prop ,namespace)
                                    `(wmi-query client ,wmi-class ,wmi-prop))))
                prop-lists wmi-properties)
             client
             (let (,@(mapcar (lambda (prop-list) `(,prop-list (car ,prop-list)))
                             prop-lists))
               (pad-lists ,@prop-lists)
               (db:with-db
                   (let* ((computer (computer client))
                          (old-objects (clsql:select ,db-class :flatp t
                                                     :where [= [computer-id] (id computer)]))
                          (new-objects (mapcar
					(lambda (,@prop-vars)
					  ,(let ((object-creator `(make-instance ,db-class
								   :computer-id (id computer))))
						(mapcar
						 (lambda (prop-var db-prop)
						   (setf object-creator
							 (append object-creator (list db-prop prop-var ))))
						 prop-vars db-props)
						object-creator))
					,@prop-lists)))
                     (unless (lists-match old-objects new-objects)
                       (multiple-value-bind (added removed) (set-differences new-objects old-objects :test #'strict-equiv)
                         (db:with-restarting-transaction
                           (when removed
                             (clsql:delete-records :from ,db-table
                                                   :where [and
							   [in [id] (mapcar #'id removed)]
							   [= [computer-id] (id computer)]]))
                           (dolist (new-obj added)
                             (setf (slot-value new-obj 'clsql-sys::view-database) nil) ; keep our restarts from messing up
                             (clsql:update-records-from-instance new-obj)))
                         ,(when added-summary
				`(when (old-computer-p client)
				  (multiple-value-bind (added removed) (set-differences added removed :test #'equiv)
				    (dolist (added-obj added)
				      (clsql:update-records-from-instance
				       (make-instance 'event :computer-id (id computer)
				                      :company-id (company-id computer)
						      :timestamp (clsql:get-time)
						      :summary ,added-summary
						      :description (format nil ,added-description
									   ,@(mapcar
									      (lambda (prop)
										(if (eql prop :computer-name)
										    `(name computer)
										    `(,prop added-obj)))
									      added-props))
						      :severity-id 0)))
				    (dolist (removed-obj removed)
				      (clsql:update-records-from-instance
				       (make-instance 'event :computer-id (id computer)
				                      :company-id (company-id computer)
						      :timestamp (clsql:get-time)
						      :summary ,removed-summary
						      :description (format nil ,removed-description
									   ,@(mapcar
									      (lambda (prop)
										(if (eql prop :computer-name)
										    `(name computer)
										    `(,prop removed-obj)))
									      removed-props))
						      :severity-id 0))))))))
		     (list ,@prop-lists))))))))))


(defpropquery query-video
  "video-controller" ("name" "refresh-rate" "horizontal-resolution" "vertical-resolution" "driver")
  'video-controller "video_controllers" 
  (:name :refresh-rate :horizontal-resolution :vertical-resolution :driver)
   "Video card added" "Video card '~a' was added to ~a" (name :computer-name)
  "Video card removed" "Video card '~a' was removed from ~a" (name :computer-name))

(defpropquery query-services
    "service" ("name")
    'service "service"
    (:name)
    "Service added" "The service '~a' was added to ~a" (name :computer-name)
    "Service removed" "The service '~a' was removed from ~a" (name :computer-name))

(defpropquery query-os
  "os" ("name" "build-number" "serial-number" "registered-user" "service-pack")
  'operating-system "operating_systems" 
  (:name :version :product-id :registered-user :service-pack))

(defpropquery query-antivirus
  "antivirus" ("name" "manufacturer" "company" "version" "up-to-date")
  'antivirus "antivirus" 
  (:name :manufacturer :company :version :up-to-date))

(defpropquery query-firewall
  ("FirewallProduct" "ROOT\\SecurityCenter") ("displayName" "companyName" "versionNumber" "enabled")
  'firewall "firewall"
  (:name :manufacturer :version :enabled))

(defpropquery query-user-accounts
  "Win32_UserAccount WHERE LocalAccount=true" ("name" "disabled")
  'user-account "user_accounts" (:name :disabled)
  "User account added" "The user '~a' was added to ~a" (name :computer-name)
  "User account removed" "The user '~a' was removed from ~a" (name :computer-name))

(defpropquery query-processor
  "processor" ("name" "speed" "cache-size" "architecture" "info")
  'processor "processors" (:name :clock-speed :l2-cache :architecture :info)
  "Processor added" "The processor '~a' was added to '~a'" (name :computer-name)
  "Processor removed" "The processor '~a' was removed from '~a'" (name :computer-name))

(defpropquery query-motherboard 
  "motherboard" ("name" "manufacturer" "serial-number")
  'motherboard "motherboards" (:name :manufacturer :serial-number)
  "Motherboard added" "The motherboard '~a' with serial number '~a' was added to ~a" (name serial-number :computer-name)
  "Motherboard removed"  "The motherboard '~a' with serial number '~a' was removed from ~a" (name serial-number :computer-name))

(defpropquery query-bios ; need to add release-date once we get dates working
  "bios" ("name" "language" "manufacturer" "version" "serial-number")
  'bios "bios" (:name :language :manufacturer :version :serial-number)
  "Bios added" "The bios '~a' version '~a' was installed on ~a" (name version :computer-name)
  "Bios removed" "The bios '~a' version '~a' was uninstalled from ~a" (name version :computer-name))

(defpropquery query-cd 
  "cdrom" ("name" "drive" "manufacturer")
  'cd-rom "cd_roms" (:name :drive :manufacturer)
  "CD Drive added" "The cd-rom drive '~a' was added to drive ~a on ~a" (name drive :computer-name)
  "CD Drive removed" "The cd-rom drive '~a' was removed from drive ~a on ~a" (name drive :computer-name))

(defpropquery query-hard-drives
  "Win32_DiskDrive" ("Caption" "manufacturer" "size" "InterfaceType")
  'hard-drive "hard_drives" (:name :manufacturer :size :interface-type)
  "Hard drive added" "The hard drive '~a' was added to ~a" (name :computer-name)
  "Hard drive removed" "The hard drive '~a' was added ~a" (name :computer-name))

(defpropquery query-logical-drives
  "Win32_LogicalDisk WHERE DriveType=3" ("Caption" "Size" "FreeSpace")
  'logical-drive "logical_drives" (:name :size :free-space))
;  "Hard drive added" "The hard drive '~a' was added to ~a" (name :computer-name)
;  "Hard drive removed" "The hard drive '~a' was added ~a" (name :computer-name))

(defpropquery query-hardware-errors
  "system" ("errors")
  'hardware-error "hardware_errors" (:description)
  "Hardware error found" "Hardware error ~a found on ~a" (description :computer-name)
  "Hardware error fixed" "Hardware error ~a fixed on ~a" (description :computer-name))

(defpropquery query-sound-device
  "sound-device" ("name" "manufacturer")
  'sound-device "sound_devices" (:name :manufacturer)
  "Sound device added" "Sound device '~a' added to ~a" (name :computer-name)
  "Sound device removed" "Sound device '~a' removed from ~a" (name :computer-name))


;;; AIEEEEE this is a macro expansion replacement to get around the fact that
;;; the form-factor call translates the integer to a string on the client side
;(defpropquery query-memory
;  "Win32_PhysicalMemory" ("Capacity" "Manufacturer" "FormFactor" "Speed")
;  'memory "memory" (:capacity :manufacturer :form-factor :memory-speed)
;  "Memory added" "Memory by '~a' was added to ~a" (manufacturer :computer-name)
;  "Memory removed" "Memory by '~a' was removed from ~a" (manufacturer :computer-name))

(defgeneric QUERY-MEMORY (CLIENT)
  (:METHOD ((CLIENT CLIENT))
    (HANDLER-CASE
      (query-let ((cap (WMI-QUERY CLIENT "Win32_PhysicalMemory" "Capacity"))
                  (man (WMI-QUERY CLIENT "Win32_PhysicalMemory" "Manufacturer"))
                  (form (WMI-QUERY CLIENT "memory" "form-factor"))
                  (loc (WMI-QUERY CLIENT "Win32_PhysicalMemory" "DeviceLocator"))
                  (speed (WMI-QUERY CLIENT "Win32_PhysicalMemory" "Speed")))
                 client
                 (let ((cap (car cap))
                       (man (car man))
                       (form (car form))
                       (loc (car loc))
                       (speed (car speed)))
                   (PAD-LISTS cap man form loc speed)
                   (WITH-DB
                       (let* ((COMPUTER (COMPUTER CLIENT))
                              (OLD-OBJECTS
                                (CLSQL-SYS:SELECT 'MEMORY :FLATP T :WHERE
                                                  (CLSQL-SYS:SQL-=
                                                    [computer-id]
                                                    (ID COMPUTER))))
                              (NEW-OBJECTS
                                (MAPCAR
                                  (LAMBDA (cap man form loc speed)
                                          (MAKE-INSTANCE 'MEMORY :COMPUTER-ID (ID COMPUTER)
                                                         :CAPACITY cap :MANUFACTURER
                                                         man :FORM-FACTOR form :LOCATION loc
                                                         :MEMORY-SPEED speed))
                                  cap man form loc speed)))
                         (unless (lists-match old-objects new-objects)
                           (multiple-value-bind (added removed) (set-differences new-objects old-objects :test #'strict-equiv)
                             (db:with-restarting-transaction
                               (when removed
                                 (clsql:delete-records :from "memory"
                                                       :where [and
                                                       [in [id] (mapcar #'id removed)]
                                                       [= [computer-id] (id computer)]]))
                               (dolist (new-obj added)
                                 (setf (slot-value new-obj 'clsql-sys::view-database) nil) ; keep our restarts from messing up
                                 (clsql:update-records-from-instance new-obj)))
                             (when (old-computer-p client)
                               (multiple-value-bind (added removed) (set-differences added removed :test #'equiv)
                                 (dolist (ADDED-OBJ ADDED)
                                   (declare (ignore added-obj))
                                   (CLSQL-SYS:UPDATE-RECORDS-FROM-INSTANCE
                                     (MAKE-INSTANCE 'EVENT :COMPUTER-ID (ID COMPUTER)
                                                    :company-id (company-id computer)
                                                    :TIMESTAMP (CLSQL-SYS:GET-TIME) :SUMMARY
                                                    "Memory added" :DESCRIPTION
                                                    (FORMAT NIL
                                                            "Memory was added to ~a"
                                                            (NAME COMPUTER))
                                                    :SEVERITY-ID 0)))
                                 (dolist (REMOVED-OBJ REMOVED)
                                   (declare (ignore removed-obj))
                                   (CLSQL-SYS:UPDATE-RECORDS-FROM-INSTANCE
                                     (MAKE-INSTANCE 'EVENT :COMPUTER-ID (ID COMPUTER)
                                                    :company-id (company-id computer)
                                                    :TIMESTAMP (CLSQL-SYS:GET-TIME) :SUMMARY
                                                    "Memory removed" :DESCRIPTION
                                                    (FORMAT NIL
                                                            "Memory was removed from ~a"
                                                            (NAME COMPUTER))
                                                    :SEVERITY-ID 0)))))))
                       (LIST cap man form loc speed))))))))


(defpropquery query-memory-array
  "Win32_PhysicalMemoryArray" ("MaxCapacity" "MemoryDevices" "MemoryErrorCorrection")
  'memory-array "memory_arrays" (:max-capacity :num-slots :error-correction))


(defpropquery query-network-card
  "Win32_NetworkAdapter WHERE NOT Manufacturer = 'Microsoft' AND NOT Name = 'RAS Async Adapter'" ("Name" "Manufacturer" "MACAddress")
  'network-card "network_cards" (:name :manufacturer :mac-address)
  "Network card added" "Network card '~a' was added to ~a" (name :computer-name)
  "Network card removed" "Network card '~a' was removed from ~a" (name :computer-name))

(defpropquery query-printer
  "printer" ("name" "is-default")
  'printer "printers" (:name :is-default)
  "Printer added" "The printer '~a' was added to ~a" (name :computer-name)
  "Printer removed" "The printer '~a' was removed from ~a" (name :computer-name))

(defpropquery query-startups
  "startup-item" ("name" "location" "command" "user")
  'startup "startups" (:name :location :command :user)
  "Startup item added" "'~a' was set to run at startup on ~a" (command :computer-name)
  "Startup item removed" "'~a' will no longer run at startup on ~a" (command :computer-name))

(defpropquery query-hot-fixes
  "hot-fix" ("name" "installed-by")
  'hot-fix "hotfixes" (:name :installed-by)
  "Hotfix installed" "The hotfix '~a' was installed on ~a" (name :computer-name)
  "Hotfix uninstalled" "The hotfix '~a' was removed from ~a" (name :computer-name))

;; Because software is a company property and not a computer property, it needs to be
;;  handled specially.
(defgeneric query-software (client)
  (:method ((client client))
    (handler-case
      (query-let
        ((result '(installed-software))
         (os-name (wmi-query client "os" "name"))
         (os-version (wmi-query client "os" "build-number")))
        client
        (db:with-db
         (when (first (first os-name))
           (push (list (first (first os-name)) "" (first (first os-version))) result))
         (let* ((computer (computer client))
                (old-objects (clsql:select 'software-computer-link :flatp t
                                           :where [= [computer-id] (id computer)]))
                (new-objects (mapcar
                              (lambda (info)
                                (let* ((name (first info))
                                       (publisher (second info))
                                       (version (third info))
                                       (software (car (clsql:select 'software :refresh t :flatp t
                                                                    :where [and [= [name] name]
                                                                    [= [company-id]
                                                                    (company-id computer)]]))))
                                  (unless software
                                    (setf software (make-instance 'software :name name
                                                                  :publisher publisher
                                                                  :company-id (company-id computer)))
                                    (insert-and-update software))
                                  (make-instance 'software-computer-link
                                                 :computer-id (id computer)
                                                 :software-id (id software)
                                                 :version version)))
                              (remove-if (lambda (info) (equal (first info) ""))
                                         (remove-duplicates result
                                                            :test (lambda (x y)
                                                                    (equal (first x) (first y))))))))
           (unless (lists-match old-objects new-objects)
             (multiple-value-bind (added removed) (set-differences new-objects old-objects :test #'strict-equiv)
               (db:with-restarting-transaction
                (when removed
                  (clsql:delete-records :from "software-computer-link"
                                        :where [and
                                        [in [id] (mapcar #'id removed)]
                                        [= [computer-id] (id computer)]]))
                (dolist (new-obj added)
                  (setf (slot-value new-obj 'clsql-sys::view-database) nil) ; keep our restarts from messing up
                  (clsql:update-records-from-instance new-obj)))
               (when (old-computer-p client)
                 (multiple-value-bind (added removed) (set-differences added removed :test #'equiv)
                   (dolist (added-obj added)
                     (clsql:update-records-from-instance
                      (make-instance 'event :computer-id (id computer)
                                     :company-id (company-id computer)
                                     :timestamp (clsql:get-time)
                                     :summary "Software installed"
                                     :description (format nil "The software '~a' was installed on ~a"
                                                          (name added-obj)
                                                          (name computer))
                                     :severity-id 0))
                     (clsql:update-records-from-instance
                      (make-instance 'software-event :computer-id (id computer)
                                     :timestamp (clsql:get-time)
                                     :installed t
                                     :name (name added-obj))))
                   (dolist (removed-obj removed)
                     (clsql:update-records-from-instance
                      (make-instance 'event :computer-id (id computer)
                                     :company-id (company-id computer)
                                     :timestamp (clsql:get-time)
                                     :summary "Software removed"
                                     :description (format nil "The software '~a' was removed from ~a"
                                                          (name removed-obj)
                                                          (name computer))
                                     :severity-id 0))
                     (clsql:update-records-from-instance
                      (make-instance 'software-event :computer-id (id computer)
                                     :timestamp (clsql:get-time)
                                     :installed nil
                                     :name (name removed-obj))))))
               result))))))))


(defun marshall-to-software-computer-link (prop-list)
  (mapcar
    (lambda (prop-list)
      (let ((software-id (first prop-list))
            (version (second prop-list))
            (id (third prop-list))
            (computer-id (fourth prop-list)))
        (make-instance 'software-computer-link
                       :software-id software-id
                       :version version
                       :id id
                       :computer-id computer-id
                       :view-database clsql:*default-database*)))
    prop-list))

(defgeneric query-software-licenses (client)
  (:method ((client client))
    (handler-case
      (query-let 
        ((keys '(software-licenses)))
        client
	  (let ((computer (computer client)))
	    (db:with-db
		(db:with-restarting-transaction
		    (mapcar
		     (lambda (license-entry)
		       (if (search "Microsoft Windows" (first license-entry))
		         (let ((scl-entry 
		                (marshall-to-software-computer-link
		                 (clsql:select 
		                  [slot-value 'software-computer-link 'software-id]
		                  [slot-value 'software-computer-link 'version]
		                  [slot-value 'software-computer-link 'id]
		                  [slot-value 'software-computer-link 'computer-id]
		                  :from (list [software]) :limit 1
		                  :inner-join [software-computer-link]
		                  :on [= [slot-value 'software-computer-link 'software-id] [slot-value 'software 'id]]
		                  :where [and [= [slot-value 'software-computer-link 'computer-id] (id computer)]
		                  [like [slot-value 'software 'name] (format nil "~a%" (first license-entry))]])))
		               (os-entry (clsql:select 'operating-system :flatp t :limit 1
		                                       :where [= [computer-id] (id computer)])))
		           (when scl-entry
		             (setf (product-id (car scl-entry)) (second license-entry))
		             (setf (license-key (car scl-entry)) (third license-entry))
		             (clsql:update-records-from-instance (car scl-entry)))
		           (when os-entry
		             (setf (license-key (car os-entry)) (third license-entry))
		             (clsql:update-records-from-instance (car os-entry))))
		       (let ((scl-entry 
			      (marshall-to-software-computer-link
			       (clsql:select 
				[slot-value 'software-computer-link 'software-id]
				[slot-value 'software-computer-link 'version]
				[slot-value 'software-computer-link 'id]
				[slot-value 'software-computer-link 'computer-id]
				:from (list [software]) :limit 1
				:inner-join [software-computer-link]
				:on [= [slot-value 'software-computer-link 'software-id] [slot-value 'software 'id]]
				:where [and [= [slot-value 'software-computer-link 'computer-id] (id computer)]
				[= [slot-value 'software 'name] (first license-entry)]]))))
			 (when scl-entry
			   (setf (product-id (car scl-entry)) (second license-entry))
			   (setf (license-key (car scl-entry)) (third license-entry))
			   (clsql:update-records-from-instance (car scl-entry))))))
		     keys))))))))

(defgeneric query-ip-address (client)
  (:method ((client client))
    (query-let ((result '(ip-address)))
               client
		 (let ((computer (computer client)))
		   (db:with-db
		       (db:with-restarting-transaction
			   (clsql:delete-records :from "ip-address" :where [= [computer-id] (id computer)])
			 (mapcar
			  (lambda (addr)
			    (clsql:update-records-from-instance
			     (make-instance 'ip-address
					    :computer-id (id computer)
					    :name addr)))
			  (remove-duplicates result :test #'equal))))
		   result))))
  
(defgeneric query-all-props (client)
  (:method ((client client))
    ;; this queues up a whole series of queries to the client
    ;; not sure what the implications for this might be yet
    (send-alerts client)
    (query-os client)
    (query-antivirus client)
    (query-firewall client)
    (query-software client)
    (query-software-licenses client)
    (query-hot-fixes client)
    (query-ip-address client)
    (query-memory-array client)
    (query-memory client)
    (query-processor client)
    (query-hard-drives client)
    (query-logical-drives client)
    (query-video client)
    (query-cd client)
    (query-printer client)
    (query-network-card client)
    (query-bios client)
    (query-motherboard client)
    (query-services client)
    (query-sound-device client)
    (query-hardware-errors client)
))

#.(clsql:restore-sql-reader-syntax-state)
