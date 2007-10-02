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

(defaction goto-warranty-report-page ((page paragent-component))
  (call 'warranty-report-page :user (user page)))

(defcomponent warranty-report-page (paragent-report-component)
  ()
  (:default-initargs
   :title "Warranty Report"))

(defrender ((page warranty-report-page))
  (report-contents page))

(defmethod render-tasks ((page warranty-report-page))
  (<:h1 "Tasks")
  (<:ul
   (<:li (link-to-prefs "Manage User Accounts"))
   (<:li (link-to-alerts "Manage Alert Settings"))))

(defgeneric render-computer-list (page computers level))

(defmethod render-computer-list ((page warranty-report-page) computers level)
  (declare (list computers))
  (dolist (computer computers)
    (<:tr :class level
          (<:td
            (<:a :href (computer-link computer)
                 (<:img :src "images/blank.gif" :width 9 :height 9)
                 (<:ah (name computer))))
          (<:td
            :align "right"
            (<:span :class "timestamp"
                    (<:ah (clsql:print-date (clsql-sys::date->time (warranty computer)) :long-day)))))))

(defmethod report-contents ((page warranty-report-page))
  (let* ((today (get-date))
         (month-later (date+ today (make-duration :month 1)))
         (half-year-later (date+ today (make-duration :month 6)))
         (expired-already
           (select 'computer :refresh t :flatp t :order-by [alias]
                   :where [and
                   [= [company-id] (company-id (user page))]
                   [<= [warranty] today]]))
         (expiring-this-month
           (select 'computer :refresh t :flatp t :order-by [alias]
                   :where [and
                   [= [company-id] (company-id (user page))]
                   [> [warranty] today]
                   [<= [warranty] month-later]]))
         (expiring-in-6-months
           (select 'computer :refresh t :flatp t :order-by [alias]
                   :where [and
                   [= [company-id] (company-id (user page))]
                   [> [warranty] month-later]
                   [<= [warranty] half-year-later]])))
    (if (and (not (or expired-already expiring-this-month expiring-in-6-months))
             ;if nothing comes up, check that warranties are even set
             (not (select 'computer :refresh t :flatp t :limit 1
                          :where [and
                          [= [company-id] (company-id (user page))]
                          [not [is [warranty] nil]]])))
        (<:div :class "report-error"
               (<:h1 "Unable to find any warranty information")
               (<:p "It does not appear that you have any warranty expiration information entered for your computers. You can add this information in the " (<:a :href "computers.ucw" "computers") " tab."))
        (progn
          (let* ((total (db-count 'computers :where [= [company-id] 
						       (company-id (user page))]))
                 (num-expired (length expired-already))
                 (num-expiring-month (length expiring-this-month))
                 (num-expiring-6-months (length expiring-in-6-months)))
            (<:div :class "box-title"
                   (<:h2 :class "report" "Warranty Report"))
            (<:div :class "box"
            (<:div 
	     :class "graph"
	     (<pie-chart page (list (unless (zerop num-expired)
				      (list (chart::get-color :high-circle) 
					    "Expired" 
					    (/ num-expired total)))
				    (unless (zerop num-expiring-month)
				      (list (chart::get-color :medium-circle) 
					    "Expiring this month" 
					    (/ num-expiring-month total)))
				    (unless (zerop num-expiring-6-months)
				      (list (chart::get-color :low-circle) 
					    "Expiring in 6 months" 
					    (/ num-expiring-6-months total)))))))
            (if expired-already
                (progn
                  (<:div :class "box-title"
                         (<:h2 "The following warranties are expired:"))
                  (<:div
                   :class "box"
                   (<:table
                    :class "warranty-table"
                    (render-computer-list page expired-already "High"))))
                (progn
                  (<:div :class "box-title"
                         (<:h2 "No warranties are expired."))
                  (<:div :class "box" )))
            (<:br)
            (if expiring-this-month
                (progn
                  (<:div :class "box-title"
                         (<:h2 "The following warranties will expire within a month:"))
                  (<:div
                   :class "box"
                   (<:table 
                    :class "warranty-table"
                    (render-computer-list page expiring-this-month "Medium"))))
                (progn
                  (<:div :class "box-title"
                         (<:h2 "No warranties will expire within the next month"))
                  (<:div :class "box")))
            (<:br)
            (if expiring-in-6-months
                (progn
                  (<:div :class "box-title"
                         (<:h2 "The following warranties will expire within half a year"))
                  (<:div
                   :class "box"
                   (<:table 
                    :class "warranty-table"
                    (render-computer-list page expiring-in-6-months "Low"))))
                (progn
                  (<:div :class "box-title"
                         (<:h2 "No warranties will expire within the next 6 months"))
                  (<:div :class "box"))))))))

#.(clsql:restore-sql-reader-syntax-state)
