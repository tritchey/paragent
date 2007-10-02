#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

(in-package :db)

#.(clsql:locally-enable-sql-reader-syntax)



;;; Alerts are so large that we put them in their own file
;;; They are nasty beasts and hellish to implement, but also very imprtant to users,
;;;  so we do what we must




(def-view-class alert (company-property)
  ((enabled :accessor enabled
            :type boolean
            :db-constraints :not-null
            :initarg :enabled
            :initform t)
   (type-id :accessor type-id
            :db-constraints :not-null
            :type integer
            :initarg :type-id)
   (email-to :accessor email-to
             :type string
             :initarg :email-to
             :initform "")
   (note :accessor note
         :type string
         :initarg :note
         :initform "")
   (severity :accessor severity
             :type integer
             :initarg :severity
             :initform 0)
   (args :accessor args
         :type list
         :initarg :args
         :initform nil)
   (severity-type :accessor severity-type
                  :db-kind :join
                  :db-info (:join-class event-type
                            :home-key severity
                            :foreign-key id
                            :set nil))
   (computers 
     :db-kind :join
     :db-info (:join-class alert-computer-link
               :home-key id
               :foreign-key alert-id
               :set t))
   (events
     :db-kind :join
     :db-info (:join-class alert-event-link
               :home-key id
               :foreign-key alert-id
               :set t))
   (impl :db-kind :virtual
         :documentation "There are in fact many classes of alert, which is not something that
 a database really understands. So we use this slot to implement our class behavior.
This slot will be filled with a subclass of alert-impl.
All dispatching can thus be done on this slot."))
  (:base-table alerts))

(defgeneric impl (alert)
  (:documentation "Returns actual object implementation of this particular type of alert."))

(defmethod impl ((alert alert))
  (if (slot-boundp alert 'impl)
      (slot-value alert 'impl)
      (bind-impl alert)))

(def-view-class alert-property (db-obj)
  ((alert-id :accessor alert-id
             :db-constraints :not-null
             :type integer
             :initarg :alert-id)
   (alert :accessor alert
          :initarg :alert
          :db-kind :join
          :db-info (:join-class alert
                    :home-key alert-id
                    :foreign-key id
                    :set nil)))
  (:documentation "Base class for anything that links to alerts"))

(def-view-class alert-computer-link (alert-property computer-property)
  ()
  (:base-table alert-computer-link))

(def-view-class alert-event-link (alert-property)
  ((event-id :accessor event-id
             :db-constraints :not-null
             :type integer
             :initarg :event-id)
   (event :accessor event
          :initarg :event
          :db-kind :join
          :db-info (:join-class event
                    :home-key event-id
                    :foreign-key id
                    :set nil)))
  (:base-table alert-event-link))

(defmethod events ((item alert))
  (select 'event :flatp t
          :where [and
          [= [slot-value 'event 'id] [slot-value 'alert-event-link 'event-id]]
          [= [slot-value 'alert-event-link 'alert-id] (id item)]]))





;;; Alert implementations

(defclass alert-impl ()
  ((db-obj :accessor db-obj
           :initarg :db-obj
           :type alert
           :initform (error "alert-impl must have db-obj supplied")))
  (:documentation "Base class for the alert implementations.
These allow easy dispatching based on the alert type."))

(defgeneric description (alert)
  (:documentation "Returns a printable description of this alert type"))


(defmethod description ((alert alert))
  (description (impl alert)))


(defclass alert-cd (alert-impl)
  ())

(defmethod description ((alert alert-cd))
  "CD Inserted")


(defclass alert-software (alert-impl)
  ())

(defmethod description ((alert alert-software))
  "Software installed")


(defclass alert-computer-offline (alert-impl)
  ())

(defmethod description ((alert alert-computer-offline))
  "Computer went offline")

(defclass alert-computer-online (alert-impl)
  ())

(defmethod description ((alert alert-computer-online))
  "Computer came online")


;; Alerts that take an argument

(defclass alert-1arg (alert-impl)
  ((default :accessor default
     :initarg :default
     :initform 0)))

(defmethod (setf arg1) (value (alert alert-1arg))
  (setf (args (db-obj alert)) (list value)))

(defmethod arg1 ((impl alert-impl))
  (car (args (db-obj impl))))

(defmethod initialize-instance :after ((alert alert-1arg) &key)
  (with-slots (default) alert
              (setf (arg1 alert) (first (args (db-obj alert))))
              (if (not (arg1 alert))
                  (progn 
                    (setf (arg1 alert) default)))))


(defclass alert-hard-drive (alert-1arg)
  ()
  (:default-initargs :default 10))

(defmethod description ((alert alert-hard-drive))
  (format nil "Hard drive space below ~a%" (arg1 alert)))


(defclass alert-processor (alert-1arg)
  ()
  (:default-initargs :default 90))

(defmethod description ((alert alert-processor))
  (format nil "Processor usage above ~a%" (arg1 alert)))


(defclass alert-memory (alert-1arg)
  ()
  (:default-initargs :default 90))

(defmethod description ((alert alert-memory))
  (format nil "Memory usage above ~a%" (arg1 alert)))


(defclass alert-service-stop (alert-1arg)
  ()
  (:default-initargs :default nil))

(defmethod description ((alert alert-service-stop))
  (if (arg1 alert)
      (format nil "Service named '~a' stopped" (arg1 alert))
      "Service stopped"))


(defclass alert-service-start (alert-1arg)
  ()
  (:default-initargs :default nil))

(defmethod description ((alert alert-service-start))
  (if (arg1 alert)
      (format nil "Service named '~a' started" (arg1 alert))
      "Service started"))


(defclass alert-process-stop (alert-1arg)
  ()
  (:default-initargs :default nil))

(defmethod description ((alert alert-process-stop))
  (format nil "Process named '~a' stopped" (arg1 alert)))

(defclass alert-pnp-added (alert-impl)
  ())

(defmethod description ((alert alert-pnp-added))
  "Plug-and-play device added")

(defclass alert-pnp-removed (alert-impl)
  ())

(defmethod description ((alert alert-pnp-removed))
  "Plug-and-play device removed")

(defclass alert-user-logon (alert-impl)
  ())

(defmethod description ((alert alert-user-logon))
  "User logged on")

(defclass alert-user-logoff (alert-impl)
  ())

(defmethod description ((alert alert-user-logoff))
  "User logged off")


(defconstant +alert-cd+ 0)
(defconstant +alert-hard-drive+ 1)
(defconstant +alert-processor+ 2)
(defconstant +alert-memory+ 3)
(defconstant +alert-software+ 4)
(defconstant +alert-service-stop+ 5)
(defconstant +alert-service-start+ 6)
(defconstant +alert-process-stop+ 7)
(defconstant +alert-computer-offline+ 8)
(defconstant +alert-pnp-added+ 9)
(defconstant +alert-pnp-removed+ 10)
(defconstant +alert-user-logon+ 11)
(defconstant +alert-user-logoff+ 12)

(defparameter *alert-types*
  (list +alert-cd+ +alert-hard-drive+ +alert-processor+ +alert-memory+ +alert-software+
        +alert-service-stop+ +alert-service-start+ +alert-process-stop+ +alert-computer-offline+ 
	+alert-pnp-added+ +alert-pnp-removed+ +alert-user-logon+ +alert-user-logoff+))

(defmethod bind-impl ((alert alert))
  (setf (slot-value alert 'impl)
        (case (type-id alert)
              (#.+alert-cd+ (make-instance 'alert-cd :db-obj alert))
              (#.+alert-hard-drive+ (make-instance 'alert-hard-drive :db-obj alert))
              (#.+alert-processor+ (make-instance 'alert-processor :db-obj alert))
              (#.+alert-memory+ (make-instance 'alert-memory :db-obj alert))
              (#.+alert-software+ (make-instance 'alert-software :db-obj alert))
              (#.+alert-service-start+ (make-instance 'alert-service-start :db-obj alert))
              (#.+alert-service-stop+ (make-instance 'alert-service-stop :db-obj alert))
              (#.+alert-process-stop+ (make-instance 'alert-process-stop :db-obj alert))
              (#.+alert-computer-offline+ (make-instance 'alert-computer-offline :db-obj alert))
	      (#.+alert-pnp-added+ (make-instance 'alert-pnp-added :db-obj alert))
	      (#.+alert-pnp-removed+ (make-instance 'alert-pnp-removed :db-obj alert))
              (#.+alert-user-logon+ (make-instance 'alert-user-logon :db-obj alert))
              (#.+alert-user-logoff+ (make-instance 'alert-user-logoff :db-obj alert))
              )))


(defmethod computers ((item alert))
  (select 'computer :flatp t :order-by [slot-value 'computer 'name] :where [and
          [= [slot-value 'computer 'id] [slot-value 'alert-computer-link 'computer-id]]
          [= [slot-value 'alert-computer-link 'alert-id] (id item)]]
          :refresh t))


(defmethod alerts ((item computer))
  (select 'alert :where [and
          [= [slot-value 'alert-computer-link 'computer-id] (id item)]
          [= [slot-value 'alert 'id] [slot-value 'alert-computer-link 'alert-id]]]
          :refresh t :flatp t))

(defmethod type-alerts ((item computer) type)
  (select 'alert :where [and
          [= [slot-value 'alert-computer-link 'computer-id] (id item)]
          [and [= [slot-value 'alert 'id] [slot-value 'alert-computer-link 'alert-id]]
          [= [slot-value 'alert 'type-id] type]]]
          :refresh t :flatp t))






#.(clsql:restore-sql-reader-syntax-state)
