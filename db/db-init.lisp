#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#


;;; Contains various functions to intiailize the database. Many are obsolete.

(in-package :db)

#.(clsql:locally-enable-sql-reader-syntax)


(defun create-tables ()
    (mapcar
     (lambda (view-class)
       (create-view-from-class view-class)
       view-class)
     *db-classes*))

(defun drop-tables ()
    (mapcar
     (lambda (view-class)
       (drop-view-from-class view-class)
       view-class)
     *db-classes*))


(defun create-groups ()
  (with-db
    (dolist (company (select 'company :flatp t))
      (update-records-from-instance
        (make-instance 'group :name "Default"
                       :remote-permission nil :shutdown-permission nil
                       :note-permission t
                       :all-computers t
                       :company-id (id company)))
      (let ((tech-group (make-instance 'group :name "Technicians"
                                       :remote-permission t :shutdown-permission t :note-permission t
                                       :all-computers t
                                       :company-id (id company))))
        (insert-and-update tech-group)
        (dolist (tech (select 'user :flatp t
                              :where [and
                              [= [company-id] (id company)]
                              [= [level] 5]]))
          (update-records-from-instance
            (make-instance 'group-user-link :group-id (id tech-group) :user-id (id tech))))))))



(defun generate-secret (company-name)
  (let ((rs (make-random-state t)))
    (format nil "~a~a~a" company-name (random 10000 rs) (random 1000 rs))))

(defmethod hash-password ((username string) (password string))
  (ironclad:byte-array-to-hex-string
    (ironclad:digest-sequence 
      :sha256
      (ironclad:ascii-string-to-byte-array (concatenate 'string username password)))))

(defmethod hash-password ((user user) (password string))
  (ironclad:byte-array-to-hex-string
    (ironclad:digest-sequence 
      :sha256
      (ironclad:ascii-string-to-byte-array (concatenate 'string (username user) password)))))

(defun add-company (company-name admin-username admin-password admin-email)
  "Creates a new company. Returns the company secret"
  (let* ((secret (generate-secret company-name))
         (company (make-instance 'company :name company-name :secret secret)))
    (insert-and-update company)
    (update-records-from-instance
      (make-instance 'user :username admin-username :email admin-email 
                     :password (hash-password admin-username admin-password)
                     :level 10 :company-id (id company)))
    (let ((license-scheme (make-instance 'license-scheme
                                         :company-id (id company)
                                         :name "Default")))
      (update-records-from-instance license-scheme))
    (update-records-from-instance
      (make-instance 'group :name "Default"
                     :remote-permission nil :shutdown-permission nil
                     :note-permission t
                     :all-computers t
                     :company-id (id company)))
    (update-records-from-instance
      (make-instance 'group :name "Technicians"
                     :remote-permission t :shutdown-permission t :note-permission t
                     :all-computers t
                     :company-id (id company)))
    company))
    

(defun add-random-events ()
  (let ((raynor (caar (select 'computer :where [= [name] "FakeRaynor"])))
        (aldaris (caar (select 'computer :where [= [name] "Fluffles"])))
        (note (make-instance 'event-type :id 0 :name "Note"))
        (low (make-instance 'event-type :id 6 :name "Low"))
        (medium (make-instance 'event-type :id 8 :name "Medium"))
        (high (make-instance 'event-type :id 10 :name "High"))
        (rs (make-random-state t)))
    (flet ((rand-time ()
                      (time- (time+  (get-time) (make-duration :day 1))
                             (make-duration :day (random 9 rs)
                                            :hour (random 23 rs)
                                            :minute (random 60 rs)))))
                (dotimes (i (+ (random 50 rs) 20))
                  (update-records-from-instance
                    (make-instance 'event :summary "Process 'solitaire.exe' started"
                                   :description "The alert you configured was triggered"
                                   :severity-id (id high)
                                   :timestamp (rand-time)
                                   :computer-id (id raynor)
                                   :company-id (company-id raynor)))
                  (update-records-from-instance
                    (make-instance 'event :summary "New software installed."
                           :description "Software installed on Aldaris."
                           :severity-id (id note)
                           :timestamp (rand-time)
                           :computer-id (id aldaris)
                                   :company-id (company-id aldaris)))
                  (let ((event-inst (make-instance 'event :summary "CD Inserted."
                                                   :description "A CD was inserted into the computer."
                                                   :severity-id (id medium)
                                                   :timestamp (rand-time)
                                                   :computer-id (id raynor)
                                                   :company-id (company-id raynor))))
                    (insert-and-update event-inst)
                    (update-records-from-instance
                      (make-instance 'alert-event-link
                                     :event-id (id event-inst)
                                     :alert-id 1)))
                  ))))


#.(clsql:restore-sql-reader-syntax-state)
