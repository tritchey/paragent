#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#


;;;; Holds various handy functions and macros for dealing with the database.


(in-package :db)


#.(clsql:locally-enable-sql-reader-syntax)


(defvar *company-id-mutex* 
  (make-mutex)
  "Used to handle ticket ids")
(defvar +ascii-alphabet+
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890")

(defun random-password (&optional (length 8) (alphabet +ascii-alphabet+))
  (let ((rs (make-random-state t)))
    (loop with id = (make-string length)
          with alphabet-length = (length alphabet)
          for i below length
          do (setf (cl:aref id i)
                   (cl:aref alphabet (random alphabet-length rs)))
          finally (return id))))

(defun db-connect ()
  "Connects us to the database"
  (let ((ret (connect (list "" *db-database* *db-username* *db-password*) :database-type :mysql :pool nil
                      :if-exists :new :make-default nil)))
;    (start-sql-recording :database ret)
    (unless ret (format t "Somehow our attempt to connect to the database returned nil~%"))
    ret))

(defmacro with-db (&rest body)
  "Macro that wraps all of the database creation and destruction"
  (let ((connected-p (gensym)))
    `(let* ((,connected-p *default-database*)
            (*default-database* (if ,connected-p ,connected-p (db-connect))))
      (unwind-protect (progn ,@body)
	(unless ,connected-p
	  (clsql:disconnect :database clsql:*default-database*))))))



(defmacro with-restarting-transaction (&body body)
  "Automatically retries the transaction until it succeeds.
This sounds like a terrible way to deal with an error (because it is), but that's 
how the mysql docs say to handle such things."
  `(loop until
         (handler-bind 
           ((clsql-sys:sql-database-data-error #'handle-transaction-data-error))
           (restart-case
             (clsql:with-transaction
		()
               (progn ,@body t))
             (transaction-deadlock-restart 
               ())))))


(defun table-for-object (object-symbol)
  "Returns the table name for the given class"
  (clsql:view-table  (find-class object-symbol)))


(defun handle-data-error (condition)
  (let ((*connect-if-exists* :new))
    (when (= (clsql:sql-error-error-id condition) 2006)
      (format t "had a connection 2006 problem~%")
      (clsql::reconnect)
      (invoke-restart (find-restart 'archon-database-error)))))

(defmethod severity-name ((event event))
  "Returns a string describing the severity of this event. Saves us a database query."
  (if (slot-boundp event 'severity-id)
      (severity-name (severity-id event))
      "Note"))

(defmethod severity-name ((severity-id integer))
  (case severity-id
        (10 "High")
        (8 "Medium")
        (6 "Low")
        (0 "Note")
        (otherwise "Note")))

(defmethod priority-name ((ticket ticket))
  "return the priority of an ticket"
  (if (slot-boundp ticket 'priority)
      (priority-name (priority ticket))
      "Note"))

(defmethod priority-name ((priority integer))
  (case priority
    (#.+ticket-priority-note+ "Note")
    (#.+ticket-priority-low+ "Low")
    (#.+ticket-priority-medium+ "Medium")
    (#.+ticket-priority-high+ "High")
    (#.+ticket-priority-critical+ "Critical")
    (otherwise "Note")))

(defmethod get-next-ticket-id ((company company))
  (get-next-ticket-id (id company)))

(defmethod get-next-ticket-id ((company-id integer))
  (with-mutex (db::*company-id-mutex*)
    (with-db
      (with-transaction
        ()
        (let ((company-ticket-id (car (select 'company-ticket-id
                                              :flatp t :limit 1
                                              :where [= [company-id] company-id]))))
          (if company-ticket-id
              (let ((next-id (next-ticket-id company-ticket-id)))
                (setf (next-ticket-id company-ticket-id) (1+ next-id))
                (update-records-from-instance company-ticket-id)
                next-id)
              (progn
                (update-records-from-instance
                  (make-instance 'company-ticket-id
                                 :company-id company-id
                                 :next-ticket-id 2))
                1)))))))


(defun handle-transaction-data-error (condition)
  "Function to restart the transaction"
  (when (= (clsql:sql-error-error-id condition) 1213)
    (invoke-restart (find-restart 'transaction-deadlock-restart))))


(defun get-last-insert ()
  "Returns the id of the last inserted item.
Generally, you want insert-and-update, not this."
  (caar (query "select last_insert_id();")))

(defun insert-and-update (record)
  "Inserts the item into the database, and then sets the correct id on it."
    (if (slot-boundp record 'id)
	(clsql:update-records-from-instance record)
	(clsql:with-transaction
	    ()
	  (clsql:update-records-from-instance record)
	  (setf (id record) (get-last-insert)))))

(defun name= (a b)
  "Convenience function that compares the names of two objects"
  (string= (name a) (name b)))


(defmethod disabled ((user user))
  (with-db (disabled (company user))))

(defparameter +session-id-alphabet+
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890")

(defun generate-session-id (&optional (length 100) (alphabet +session-id-alphabet+))
  (let ((rs (make-random-state t)))
    (loop with id = (make-string length)
          with alphabet-length = (length alphabet)
          for i below length
          do (setf (cl:aref id i)
                   (cl:aref alphabet (random alphabet-length rs)))
          finally (return id))))

(defgeneric delete-company (company)
  (:documentation "Deletes the company from the database, along with the company's properties."))

(defmethod delete-company ((company company))
  (delete-company (id company)))

(defmethod delete-company ((company-id integer))
  (with-db
   (delete-records :from [companies] :where [= [id] company-id])
   (macrolet ((del-prop (table) `(delete-records :from ,table :where [= [company-id] company-id])))
     (del-prop [users])
     (del-prop [events])
     (del-prop [software])
     (del-prop [license-scheme])
     (del-prop [groups])
     (del-prop [alerts])
     (del-prop [tickets])
     (del-prop [ticket-emails]))
   (delete-computers (select 'computer :flatp t :where [= [company-id] company-id]))))

(defgeneric delete-computer (computer)
  (:documentation "Deletes the computer and its properties. May take some time."))

(defmethod delete-computer ((computer-id integer))
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
                  "video-controllers" "ticket_computers")))

(defgeneric delete-computers (computers)
  (:documentation "Deletes the computers and their properties. May take some time.
It is much more efficient to call this than to call delete-computer repeatedly."))

(defmethod delete-computers ((computers list))
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


;; Some clsql functions we had to override

;; Allow me to explain the following:
;;  Each database object stores a reference to the database connection it was selected from
;;  This is a problem for us, as we close down those connections constantly.
;;  The following redefines every object's database to be the current connection
;;  We return nil if the slot was not set, as this indicates that the object
;;   still has to be inserted into the db, and is thus treated differently by clsql
(defmethod clsql-sys::view-database ((obj clsql:standard-db-object))
  (if (slot-value obj 'clsql-sys::view-database)
      *default-database*
      nil))

(defun clsql-sys::sql-output (sql-expr &optional (database clsql:*default-database*))
  "Top-level call for generating SQL strings. Returns an SQL
  string appropriate for DATABASE which corresponds to the
  supplied lisp expression SQL-EXPR."
  (progv '(clsql-sys::*sql-stream*)
      `(,(clsql-sys::make-string-output-stream))
    (clsql-sys::output-sql sql-expr database)
    (clsql-sys::get-output-stream-string clsql-sys::*sql-stream*)))

(defun clsql-sys::key-qualifier-for-instance (obj &key (database clsql:*default-database*))
  (let ((tb (view-table (class-of obj))))
    (flet ((qfk (k)
             (sql-operation '==
                            (sql-expression :attribute
                                            (clsql-sys::view-class-slot-column k)
                                            :table tb)
                            (clsql-sys::db-value-from-slot
                             k
                             (slot-value obj (clsql-sys::slot-definition-name k))
                             database))))
      (let* ((keys (clsql-sys::keyslots-for-class (class-of obj)))
	     (keyxprs (mapcar #'qfk (reverse keys))))
	(cond
          ((= (length keyxprs) 0) [= [id] (id obj)])
          ((= (length keyxprs) 1) (car keyxprs))
          ((> (length keyxprs) 1) (apply #'sql-operation 'and keyxprs)))))))

(defgeneric sql-equal (a b)
  (:documentation "Tells whether two sql expression are equivalent.
We use it with many of Nexus' filters."))

(defmethod sql-equal ((a t) (b t))
  (equal a b))

(defmethod sql-equal ((a list) (b list))
  (and (equal (length a) (length b))
       (loop for item-a in a
	     for item-b in b do
	     (unless (sql-equal item-a item-b) (return))
	     finally (return t))))

(defmacro defsql-equal (type &rest slots)
  `(defmethod sql-equal ((a ,type) (b ,type))
     (and ,@(mapcar
	      (lambda (slot)
		`(sql-equal (slot-value a ,slot) (slot-value b ,slot)))
	      slots))))

(defmethod sql-equal ((a clsql-sys:sql-relational-exp) (b clsql-sys:sql-relational-exp))
  (and (sql-equal (slot-value a 'clsql-sys::operator) (slot-value b 'clsql-sys::operator))
       (sql-equal (slot-value a 'clsql-sys::sub-expressions) (slot-value b 'clsql-sys::sub-expressions))))

(defmethod sql-equal ((a clsql-sys:sql-ident-attribute) (b clsql-sys:sql-ident-attribute))
  (and (sql-equal (slot-value a 'clsql-sys::name) (slot-value b 'clsql-sys::name))
       (sql-equal (slot-value a 'clsql-sys::qualifier) (slot-value b 'clsql-sys::qualifier))
       (sql-equal (slot-value a 'clsql-sys::type) (slot-value b 'clsql-sys::type))))


(defsql-equal clsql-sys:sql-function-exp
  'clsql-sys::args 'clsql-sys::name)

(defsql-equal clsql-sys:sql-query 
  'clsql-sys::all 'clsql-sys::distinct 'clsql-sys::flatp 'clsql-sys::from
  'clsql-sys::group-by 'clsql-sys::having 'clsql-sys::inner-join
  'clsql-sys::limit 'clsql-sys::offset 'clsql-sys::on 'clsql-sys::order-by
  'clsql-sys::selections 'clsql-sys::set-operation 'clsql-sys::where)


#.(clsql:restore-sql-reader-syntax-state)
