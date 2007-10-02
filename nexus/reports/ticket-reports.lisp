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

(defcomponent tickets-report (paragent-report-component)
  ((header :accessor header
           :initarg :header
           :type string
           :initform "")))

(defgeneric tickets-query (tickets-report)
  (:documentation "Returns the list of tickets to be displayed"))

(defmethod report-contents ((page tickets-report))
  (<:script :src "sorttable.js" :type "text/javascript")
  (<:h1 :class "report" (<:ah (header page)))
  (let ((odd-row nil))
    (<:div
     :class "box-title")
    (<:div
     :class "box"
     (<:table
      :class "sortable" :id "overdue-tickets-report"
      (<:tr (<:th "Ticket") (<:th "Assigned user") (<:th "Priority") (<:th "Due Date"))
      (dolist (ticket (tickets-query page))
        (<:tr :class (if odd-row "odd-row" "even-row")
              (setf odd-row (not odd-row))
              (<:td (<:a :href (ticket-link ticket) (<:ah (subject ticket)))) 
              (<:td (<:ah (if (assigned-user ticket) 
                              (username (assigned-user ticket))
                              (<:i "None"))))
              (<:td (<:ah (priority-string ticket)))
              (<:td (<:ah (or (due-date ticket) (<:i "None"))))))
  ))))

(defcomponent tickets-overdue-report (tickets-report)
  ()
  (:default-initargs :header "Overdue Tickets"))


(defmethod tickets-query ((page tickets-overdue-report))
  (select 'ticket :flatp t
          :where [and 
          [= [company-id] (company-id (user page))]
          [= [state] +ticket-status-open+ ]
          [< [due-date] (get-date)]
          ]))

(defcomponent tickets-due-today-report (tickets-report)
  ()
  (:default-initargs :header "Tickets Due Today"))


(defmethod tickets-query ((page tickets-due-today-report))
  (select 'ticket :flatp t
          :where [and 
          [= [company-id] (company-id (user page))]
          [= [state] +ticket-status-open+ ]
          [= [due-date] (get-date)]
          ]))

(defcomponent tickets-new-report (tickets-report)
  ()
  (:default-initargs :header "New Tickets"))


(defmethod tickets-query ((page tickets-new-report))
  (select 'ticket :flatp t
          :where [and 
          [= [company-id] (company-id (user page))]
          [>= [timestamp] (get-date)]
          ]))


#.(clsql:restore-sql-reader-syntax-state)
