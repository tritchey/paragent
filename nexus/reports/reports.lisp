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

;; Report base class

(defgeneric report-contents (page)
  (:documentation "Gives the contents of the report, without all the sidebars and other cruft"))

(defcomponent paragent-report-component (paragent-window-component)
  ()
  (:default-initargs
    :title "Report"))

(defmethod body-id ((page paragent-report-component))
  "reports")

(defrender ((page paragent-report-component))
  (report-contents page))

(defcomponent reports-page (paragent-report-component)
  ())

(defrender ((page reports-page))
  (show-tip (user page)
            "Each of these reports can also be accessed at any time from the left side-bar.")
  (<:ul 
    :class "reports"
    (<:li (<:h3 (link-to-inventory "Hardware Inventory"))
          (<:p "View data about all the computers on your network.")
          (<:br))
          
    (<:li (<:h3 (link-to-software-report "Software Inventory"))
          (<:p "View a summary of your current software license compliance.")
          (<:br))
    
    (<:li (<:h3 (link-to-license-keys-report "License Keys"))
          (<:p "See the license keys for the software you have installed.")
          (<:br))
    
    (<:li (<:h3 (link-to-warranty-report "Warranties"))
          (<:p "See which warranties are about to expire.")
          (<:br))
    (<:li (<:h3 (link-to-event-report 7 "This Week's Events"))
          (<:p "See the events of the past week")
          (<:br))
    (<:li (<:h3 (link-to-event-report 30 "This Month's Events"))
          (<:p "See the events of the past month")
          (<:br))))





;;; Software licensing report

(defaction goto-software-report-page ((page paragent-component))
  (call 'software-report-page :user (user page)))

(defcomponent software-report-page (paragent-report-component)
  ()
  (:default-initargs 
   :title "Software Licenses Report"))



(set-url software-report-page *url-software-report*)

(defmethod render-tasks ((page software-report-page))
  (<:h1 "Tasks")
  (<:ul
    (<:li (link-to-license-keys-report "View License Keys"))
    (<:li (link-to-software "Edit Licenses"))))

(defrender ((page software-report-page))
  (report-contents page))

(defmethod report-contents ((page software-report-page))
    (<:h1 :class "report" "Software Report")
    (let* ((user (user page))
           (scheme (get-software-scheme user)))
      (multiple-value-bind (softwares licenseses computerss)
        (get-unlicensed-software user :scheme scheme)
        (if softwares
            (progn
              (<:h2 :class "report-warning" "You have unlicensed software")
              (<:div :class "box-title"
                     (<:h2 "The following software has insufficient licenses:"))
              (<:div
               :class "box"
               (<:table
                :width "100%"
                (let ((odd-row nil))
                  (loop for software in softwares
                        for licenses in licenseses
                        for computers in computerss do
                        (<:tr
                         :class (if odd-row "odd-row" "even-row")
                         (setf odd-row (not odd-row))
                         (<:td
                          (<:as-html (name software)))
                         (<:td
                          :align "right"
                          (<:as-html (num licenses)
                                     (if (equal (num licenses) 1) " license, " " licenses, ")
                                     (length computers) " more needed"))))))))
            (progn
              (<:h2 :class "report" "All your software is properly licensed."))))
      (let ((licenses (license-counts scheme)))
        (<:div :class "box-title"
               (<:h2 "You own the following software licenses:"))
        (<:div
         :class "box"
         (<:table
          :width "100%"
          (let ((odd-row nil))
            (dolist (license licenses)
              (let ((license license))
                (<:tr
                 :class (if odd-row "odd-row" "even-row")
                 (setf odd-row (not odd-row))
                 (<:td (<:as-html (name (software license))))
                 (<:td
                  :align "right"
                  (<:as-html (num license)
                             (if (equal (num license) 1) " license" " licenses"))))))))))))



;;; License Keys

(defcomponent license-keys-report-page (paragent-report-component)
  ()
  (:default-initargs
    :title "License Keys"))

(defrender ((page license-keys-report-page))
  (report-contents page))

(defmethod render-tasks ((page license-keys-report-page))
  (<:h1 "Tasks")
  (<:ul
    (<:li (link-to-software "View Software"))))

(defun get-software-key-infoz (software)
  (declare (software software))
  (with-db
   (select [slot-value 'software-computer-link 'license-key]
           [slot-value 'software-computer-link 'product-id]
           [slot-value 'computer 'name]
           :from (list [software-computer-link] [computers])
           :order-by [slot-value 'computer 'name]
           :distinct t
           :where [and [= [slot-value 'software-computer-link 'software-id] (id software)]
           [= [slot-value 'computer 'id] [slot-value 'software-computer-link 'computer-id]]
           [not [null [slot-value 'software-computer-link 'license-key]]]
           ])))

(defmethod report-contents ((page license-keys-report-page))
  (<:script :src "sorttable.js" :type "text/javascript")
  (let* ((user (user page))
         (softwares (select 'software :flatp t :distinct t
                            :where [and [= [slot-value 'software 'company-id] (company-id user)]
                            [= [slot-value 'software 'id] [slot-value 'software-computer-link 'software-id]]
                            [not [null [slot-value 'software-computer-link 'license-key]]]])))
    (left-right-align
     (<:h1 :class "report" "License Keys")
     (unless (guest-p user)
       (link-to-csv page
                    (loop for software in softwares append
                          (append
                           (list (list (name software))
                                 (list "Computer" "License Key" "Product ID"))
                           (mapcar
                            (lambda (info)
                              (let ((computer-name (third info))
                                    (key (first info))
                                    (id (second info)))
                                (list computer-name key id)))
                            (get-software-key-infoz software))))
                    "Export to csv")))
    (show-tip user
              "Actual license keys are hidden in this demonstration")
    (<:div
      :id "license-report"
      (dolist (software softwares)
        (let ((infoz (get-software-key-infoz software)))
          (<:div
           :class "box-title"
           (<:h2 (<:ah (name software))))
          (<:div
           :class "box"
           (<:table
            :class "sortable" :id "license-key-table"
            (<:tr (<:th "Computer") (<:th "License Key") (<:th "Product ID"))
            (let ((odd-row t))
              (dolist (info infoz)
                (<:tr
                 :class (if odd-row "odd-row" "even-row")
                 (setf odd-row (not odd-row))
                 (<:td
                  (let ((computer-name (third info)))
                    (<:a :href (computer-link computer-name)
                         (<:ah computer-name))))
                 (<:td (<:ah (if (guest-p user)
                                 "XXXXX-XXXXX-XXXXX-XXXXX-XXXXX"
                                 (first info))))
                 (<:td (<:ah (if (guest-p user)
                                 "11111-111-1111111-11111"
                                 (second info))))))))))
        (<:br)
        ))))

          
      
      





;;; Events from the past week



(defaction goto-events-report-page ((page paragent-component) &key (duration 7))
  (call 'events-report-page :user (user page) :duration duration))

(defcomponent events-report-page (paragent-report-component)
  ((duration :accessor duration
             :initarg :duration
             :type integer
             :initform 14))
  (:default-initargs 
   :title "Events Report"))

(defrender ((page events-report-page))
  (report-contents page))

(defmethod render-tasks ((page events-report-page))
  (<:h1 "Tasks")
  (<:ul
   (<:li (link-to-prefs "Manage User Accounts"))
   (<:li (link-to-alerts "Manage Alert Settings"))))

(defvar *short-day-names* '("S" "M" "T" "W" "T" "F" "S"))
(defvar *med-day-names* '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
(defvar *long-day-names* '("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))

(defun print-dow (time &optional (style :long))
  (multiple-value-bind (placeholder second minute hour day month year dow)
      (decode-time time)
    (declare (ignore placeholder second minute hour day month year))
    (ecase style
      (:short
       (format nil "~a" (nth dow *short-day-names*)))
      (:medium
       (format nil "~a" (nth dow *med-day-names*)))
      (:long
       (format nil "~a" (nth dow *long-day-names*))))))


(defmethod report-contents ((page events-report-page))
  (<:script :src "sorttable.js" :type "text/javascript")
  ;; Make sure some wily user doesn't input stupid values
  (when (> (duration page) 60)
    (setf (duration page) 60))
  (when (< (duration page) 1)
    (setf (duration page) 1))
  (with-db
    (let* ((today (get-date))
           (earlier (date- today (make-duration :day (duration page))))
           (odd-row t))
	   ;(events (select 'event :refresh t :order-by [timestamp]
           ;                :where [and [= [computer-id] [slot-value 'computer 'id]]
           ;                [= [slot-value 'computer 'company-id] (company-id (user page))]
           ;                [> [timestamp] earlier]])))
      (<:div
       :class "box-title"
       (left-right-align
        (<:h1 :class "report" "Events Report")
        (link-to-csv page nil "Export to csv"))
       (flet ((format-date (date)
                (multiple-value-bind (usec second minute hour day month year dow)
                  (decode-time (clsql-sys::date->time date))
                  (declare (ignore usec second minute hour))
                  (format nil "~a, ~a ~d, ~d" (clsql-sys::day-name dow) (month-name month) day year))))
         (<:h2 :class "report" (<:as-html (format-date earlier) " - " (format-date today)))))
      (<:div
       :class "box"
       (<:div 
        :class "graph"
        (<event-timeline-graph page (user page) :height 200 :width 500))
       (<:div :class "event-report"
              (<:h2 (<:as-html "The past " (duration page) " days' events"))
              (<:table
               :cellspacing 0 :cellpadding 0
               :class "sortable" :id "events-table"
               (<:tr
                (<:th "Description")
                (<:th "Timestamp"))
               (db-chunk (event (select [slot-value 'event 'severity-id] [slot-value 'event 'description]
                                        [slot-value 'event 'timestamp]
                                        :from [events]
                                        :flatp t :refresh t :order-by [timestamp]
                                        :where [and
                                        [= [slot-value 'event 'company-id] (company-id (user page))]
                                        [> [timestamp] earlier]]))
                 
                 (<:tr
                  :class (if odd-row "odd-row" "even-row")
                  (setf odd-row (not odd-row))
                  (<:td (<:as-html (second event)))
                  (<:td (<:as-html (third event)))))))))))
  
(defaction goto-csv ((page events-report-page) data)
  (call 'events-csv-page :duration (duration page) :user (user page)))

(defcomponent events-csv-page (csv-page paragent-component)
  ((duration :accessor duration
             :type integer
             :initarg :duration)))

(defmethod render ((page events-csv-page))
  (with-db
    (let* ((today (get-date))
           (earlier (date- today (make-duration :day (duration page)))))
      (<:ai "\"Severity\",\"Description\",\"Time\"
")
      (db-chunk (event (select [slot-value 'event 'severity-id] [slot-value 'event 'description]
                               [slot-value 'event 'timestamp]
                               :from [events]
                               :flatp t :refresh t :order-by [timestamp]
                               :where [and
                               [= [slot-value 'event 'company-id] (company-id (user page))]
                               [> [timestamp] earlier]]))
        (<:ai "\"" (escape-for-csv (severity-name (first event))) "\"" )
        (dolist (datum (cdr event))
          (<:as-is ",\"" (escape-for-csv datum) "\"" ))
        (<:as-is "
")))))


#.(clsql:restore-sql-reader-syntax-state)








