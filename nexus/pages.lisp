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


(defmacro defpage (url class)
  `(defentry-point ,url (:application *my-app*)
     ()
     (let ((user (get-user)))
       (if (and user (not (disabled user)))
           (progn
             (save-login-time user)
             (call ,class :user user))
           (progn
             (let ((user (call 'redirecting-login-page)))
               (call ,class :user user)))))))

(defmacro set-url (class url)
  `(defmethod update-url ((page ,class) url)
     (setf (ucw::uri.path url) ,url)
     url))


;;; Login

(defentry-point "index.ucw" (:application *my-app*)
  ()
  (call 'login-page))

(defentry-point "login.ucw" (:application *my-app*)
  ()
  (call 'login-page))

(defentry-point "" (:application *my-app*)
  ()
  (call 'login-page))

(defparameter *url-login* "login.ucw")



;;; Main

(defparameter *url-main* "main.ucw")

(defmacro link-to-main (&rest body)
  `(<:a :href *url-main*
        ,@body))

(defpage "main.ucw" 'front-page)



;;; Alerts

(defparameter *url-alerts* "alerts.ucw")

(defmacro link-to-alerts (&rest body)
  `(<:a :href *url-alerts*
        ,@body))

(defentry-point "alerts.ucw" (:application *my-app*) 
    ()
  (multiple-value-bind (user company) (get-user)
    (when (>=account-plus company)
      (if (and user (not (disabled user)))
          (progn
            (save-login-time user)
            (call 'alerts-page :user user))
          (PROGN
            (LET ((USER (CALL 'REDIRECTING-LOGIN-PAGE)))
              (CALL 'ALERTS-PAGE :USER USER)))))))


;;; Tickets

(defparameter *url-tickets* "tickets.ucw")

(defmacro link-to-tickets (&rest body)
  `(<:a :href *url-tickets*
        ,@body))

(defentry-point "tickets.ucw" (:application *my-app*)
  (id)
  (multiple-value-bind (user company) (get-user)
    (if (and user (not (disabled user)))
        (if (>=account-plus company)
            (let ((tickets-page (make-instance 'tickets-page :user user
                                               :ticket-id (when id (parse-integer id :junk-allowed t)))))
              (save-login-time user)
              (call-component nil tickets-page))
            (call-component 'computers-page :user user))
        (let ((user (call 'redirecting-login-page)))
          (call-component nil
                          (make-instance 'tickets-page :user user
                                         :ticket-id (when id (parse-integer id :junk-allowed t))))))))
  
(defentry-point "submit-ticket.ucw" (:application *my-app*)
  (company)
  (call 'external-ticket-page 
	:company-name company
        :company (company-for-name company)))




(defun ticket-link (ticket)
  (declare (ticket ticket))
  (format nil "ticket.ucw?id=~a" (id ticket)))


(defparameter *url-my-open-tickets* "my-open-tickets.ucw")

(defentry-point "my-open-tickets.ucw" (:application *my-app*)
  ()
  (let* ((user (get-user)))
    (if (and user (not (disabled user)))
        (progn
          (save-login-time user)
          (let* ((callee (make-instance 'tickets-page :user user))
                 (ticket-list (ticket-list callee)))
            (setf (filter ticket-list) [= [slot-value 'ticket 'state] 1])
            (set-filter ticket-list :open-closed
                        [= [slot-value 'ticket 'state] 1]
                        [tickets])
            (setf (assigned-filter ticket-list) [= [slot-value 'ticket 'assigned-user-id] (id user)])
            (set-filter ticket-list :assigned 
                        [= [slot-value 'ticket 'assigned-user-id] (id user)]
                        [tickets])
            
            (call-component nil callee)))
        (progn
          (let ((user (call 'redirecting-login-page)))
            (let* ((callee (make-instance 'tickets-page :user user))
                   (ticket-list (ticket-list callee)))
              (setf (filter ticket-list) [= [slot-value 'ticket 'state] 1])
              (set-filter ticket-list :open-closed
                          [= [slot-value 'ticket 'state] 1]
                          [tickets])
              (setf (assigned-filter ticket-list) [= [slot-value 'ticket 'assigned-user-id] (id user)])
              (set-filter ticket-list :assigned 
                          [= [slot-value 'ticket 'assigned-user-id] (id user)]
                          [tickets])
              
              (call-component nil callee)))))))

;;; Software

(defparameter *url-software* "software.ucw")

(defmacro link-to-software (&rest body)
  `(<:a :href *url-software*
        ,@body))

(defpage "software.ucw" 'software-page)


;;; Computers

  

(defparameter *url-computers* "computers.ucw")

(defmacro link-to-computers (&rest body)
  `(<:a :href *url-computers*
        ,@body))

(defmacro link-to-online-computers (&rest body)
  `(<:a :href "onlinecomputers.ucw"
        ,@body))

(defmacro link-to-offline-computers (&rest body)
  `(<:a :href "offlinecomputers.ucw"
        ,@body))

(defpage "computers.ucw" 'computers-page)

(defentry-point "computers.ucw" (:application *my-app*)
  (id)
  (let ((user (get-user)))
    (if (and user (not (disabled user)))
	(progn
	  (save-login-time user)
	  (call 'computers-page :user user
		:expanded-computer id))
	(progn
	  (let ((user (call 'redirecting-login-page)))
	    (call 'computers-page :user user 
		  :expanded-computer id))))))

(defentry-point "offlinecomputers.ucw" (:application *my-app*)
  ()
  (let ((user (get-user)))
    (if user
        (let* ((computer-page (make-instance 'computers-page :user user))
               (computer-list (computer-list computer-page)))
          (set-filter computer-list :online [not [= [slot-value 'computer 'online] t]] [computers])
          (setf (title computer-list) "Offline Computers")
          (call-component nil computer-page))
        (call 'redirect-component :target "login.ucw"))))

(defentry-point "onlinecomputers.ucw" (:application *my-app*)
  ()
  (let ((user (get-user)))
    (if user
        (let* ((computer-page (make-instance 'computers-page :user user))
               (computer-list (computer-list computer-page)))
          (set-filter computer-list :online [= [slot-value 'computer 'online] t] [computers])
          (setf (title computer-list) "Online Computers")
          (call-component nil computer-page))
        (call 'redirect-component :target "login.ucw"))))

;;; Preferences

(defparameter *url-prefs* "preferences.ucw")

(defmacro link-to-prefs (&rest body)
  `(<:a :href *url-prefs*
        ,@body))

(defpage "preferences.ucw" 'preferences-page)


;;; Advanced Search


(defmacro link-to-advanced-search (&rest body)
  `(<:a :href "advanced-search.ucw"
        ,@body))

(defpage "advanced-search.ucw" 'advanced-search-page)


(defentry-point "search.ucw" (:application *my-app*)
  ((q ""))
  (let ((user (get-user)))
    (if user
        (call 'search-page :user user :search-clause q)
        (call 'redirect-component :target "login.ucw"))))


;;; Reports

(defparameter *url-reports* "reports.ucw")

(defmacro link-to-reports (&rest body)
  `(<:a :href *url-reports*
        ,@body))

(defpage "reports.ucw" 'reports-page)

;; Inventory
(defparameter *url-inventory* "inventory.ucw")

(defmacro link-to-inventory (&rest body)
  `(<:a :href *url-inventory*
        ,@body))

(defpage "inventory.ucw" 'inventory-report-page)


;; Software report
(defparameter *url-software-report* "softwarereport.ucw")

(defmacro link-to-software-report (&rest body)
  `(<:a :href *url-software-report*
        ,@body))

(defpage "softwarereport.ucw" 'software-report-page)


;; Warranty report
(defpage "warranties.ucw" 'warranty-report-page)

(defmacro link-to-warranty-report (&rest body)
  `(<:a :href "warranties.ucw"
        ,@body))



;; Event report
(defmacro link-to-event-report (duration &rest body)
  `(<:a :href (format nil "eventreport.ucw?duration=~a" ,duration)
        ,@body))

(defentry-point "eventreport.ucw" (:application *my-app*)
  ((duration "7"))
  (let ((user (get-user)))
    (if (and user duration)
        (let ((duration (parse-integer duration :junk-allowed t)))
          (call 'events-report-page :user user :duration duration))
        (call 'redirect-component :target "login.ucw"))))



;; License keys report
(defpage "license-keys.ucw" 'license-keys-report-page)

(defmacro link-to-license-keys-report (&rest body)
  `(<:a :href "license-keys.ucw"
        ,@body))


;; Ticket reports
(defpage "overdue-tickets.ucw" 'tickets-overdue-report)

(defmacro link-to-tickets-overdue-report (&rest body)
  `(<:a :href "overdue-tickets.ucw"
        ,@body))

(defpage "tickets-due-today.ucw" 'tickets-due-today-report)

(defmacro link-to-tickets-due-today-report (&rest body)
  `(<:a :href "tickets-due-today.ucw"
        ,@body))


(defpage "tickets-new.ucw" 'tickets-new-report)

(defmacro link-to-tickets-new-report (&rest body)
  `(<:a :href "tickets-new.ucw"
        ,@body))


;; Signup page


(defentry-point "signup.ucw" (:application *my-app*)
  ()
  (call 'signup-page))



#.(clsql:restore-sql-reader-syntax-state)

