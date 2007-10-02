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


(defcomponent event-list (filtered-item-list)
  ((filter :accessor filter
           :initarg :filter
	   :type integer
           :initform -1)
   (title :accessor title
          :initarg :title
	  :type string
          :initform "Latest Events")
   (show-filter :accessor show-filter
                :initarg :show-filter
		:type boolean
                :initform t)
   (computers :accessor computers
              :type hash-table))
  (:documentation "Item-list for displaying events")
  (:default-initargs
    :div-id "latest"
    :show-title t
    :show-header nil
    :use-optimized t
    :results-per-page 5
    :default-view-classes (list [events])))

(defaction do-filter ((page event-list) filter)
  (setf (filter page) filter)
  (if (equal filter -1)
      (remove-filter page :filter)
      (set-filter page :filter [= [slot-value 'event 'severity-id] filter] [events]))
  (setf (from page) 0))

(defmethod render-title ((page event-list))
  (<:div 
   :class "box-title"
   (<:h2 (<:as-html (title page)))
   (when (show-filter page)
     (let* ((events-list (select 'event-type :flatp t))
           (filter (filter page)))
       (<:div :class "severity-filter"
       (<:ul
            (<:li :class "first-item" 
                  (if (= filter -1)
                      (<:as-html "All")
                      (<ucw:a  :action (do-filter page -1) 
                               :title "Show All"
                               (<:as-html "All"))))
            (dolist (event-type events-list)
              (<:li (if (= filter (id event-type))
                        (<:as-html (name event-type))
                        (<ucw:a :action (do-filter page (id event-type)) 
                                :title "Show Only"
                                (<:as-html (name event-type))))))))))))

(defmethod optimized-list-count ((page event-list))
  (car
    (select [count [slot-value 'event 'id]]
            :from (get-filter-tables page)
            :flatp t
            :where [and
            [= [slot-value 'event 'company-id] (company-id (user page))]
            (get-filter-sql page)])))

(defmethod optimized-list-getter ((page event-list) start len)
  (let ((events (select 'event :flatp t :refresh t
                        :from (get-filter-tables page)
                        :where [and
                        [= [slot-value 'event 'company-id] (company-id (user page))]
                        (get-filter-sql page)]
                        :offset start :limit len
                        :order-by '(([timestamp] :desc)))))
    (setf (computers page) (make-hash-table))
    (when events
      (dolist (computer (select 'computer :flatp t :limit len
                                :where [in [id] (mapcar #'computer-id events)]))
        (setf (gethash (id computer) (computers page)) computer)))
    events))
    

(defun make-event-badge (timestamp)
  (multiple-value-bind (tu ts tm th t-day t-month t-year t-dow)
      (clsql:decode-time (clsql:get-time))
    (declare (ignore tu ts tm th t-dow))
    (multiple-value-bind (u second minute hour day month year dow)
	(clsql:decode-time timestamp)
      (declare (ignore u second dow))
      (let ((date (cond
		    ((and (= year t-year) (= month t-month) (= day t-day))
		     "Today")
		    (t
		     (format nil "~d/~d/~2,'0d" month day (- year 2000))))))
	(<:as-html 
	 (<:span :class "time" (<:as-html (format nil "~d:~2,'0d" hour minute)))
	 " "
	 (<:span :class "date" (<:as-html date)))))))
		
(defun replace-computer-name (computer-name description)
  "Takes the description of an event, finds that name of the computer it points to, and 
turns it into a link to that computer."
  (declare (string computer-name description))
  (let ((start (search computer-name description)))
    (if start
	(let ((end (+ start (length computer-name))))
	  (<:ah (subseq description 0 start))
	  (<:a :href (format nil "/computer-details.ucw?id=~a" computer-name)
	       (<:ah (subseq description start end)))
	  (<:ah (subseq description end)))
	(<:ah description))))

(defmethod render-item ((page event-list) event)
  (declare (type event event))
  (flet ((render-description (name event)
           (let ((description (description event)))
             (if (search "hotfix" description)
                 (let* ((matches (cl-ppcre:all-matches "'.*'" description))
                        (start (first matches))
                        (end (second matches)))
                   (if matches
                       (progn
                         (<:ah (subseq description 0 start))
                         (<:a :href (link-for-hotfix description)
                              :target "_blank"
                              :class "hotfix"
                              (<:ah (subseq description start end)))
                         (replace-computer-name name (subseq description end)))
                       (replace-computer-name name description)))
                 (replace-computer-name name description)))))
    (let ((computer (gethash (computer-id event) (computers page)))
          (odd-row (if (odd-row page) "odd-row" "even-row")))
      (if (not (condensed page))
	  (progn
	    (<:tr
	     :class odd-row
	     (<:td :rowspan 3 :align "center" :class (format nil "~a badge" (severity-name event))
		   ;; format the timestamp
		   (make-event-badge (timestamp event)))
	     (<:td :class "summary"
		   (<:as-html (summary event))))
	    (<:tr
	     :class odd-row
	     (<:td :class "description" (render-description (name computer) event)))
	    (<:tr
	     :class odd-row
	     (<:td :class "computer" 	     
		   (when computer
		     (<ucw:a :action (goto-computers-page page (user page) :computer computer)
			     :class "computer"
			     (<:as-html (name computer)))))))
	  (progn 
	    (<:tr
	     :class odd-row
	     (<:td :rowspan 2 :align "center" :class "badge"
		   (<:img :src (format nil "/images/cond-~a.gif" (severity-name event))
			  :width "24px" :height "24px"))
	     (<:td :class "summary"
		   (<:as-html (summary event) " at ")
		   (make-event-badge (timestamp event))))
	    (<:tr
	     :class odd-row
	     (<:td :class "description" (<:p (render-description (name computer) event)))))))))

(defcomponent inline-event-list (event-list)
  ()
  (:documentation "Renders events in a more compact format"))

(defmethod render-title ((page inline-event-list))
   (<:h2 (<:as-html (title page)))
   (when (show-filter page)
     (let* ((events-list (select 'event-type :flatp t))
           (filter (filter page)))
       (<:div :class "severity-filter"
       (<:ul 
	     (<:li :class "first-item" 
		   (if (= filter -1)
		       (<:as-html "All")
		       (<ucw:a  :action (do-filter page -1) 
				:title "Show All"
				(<:as-html "All"))))
	     (dolist (event-type events-list)
	       (<:li (if (= filter (id event-type))
			 (<:as-html (name event-type))
			 (<ucw:a :action (do-filter page (id event-type)) 
				 :title "Show Only"
				 (<:as-html (name event-type)))))))))))

(defmethod render ((page inline-event-list))
  ;; Javascript
  (<:script :type "text/javascript"
            (<:as-is
              (render-js page)))
  ;; Html
  (render-title page)
  (render-html page))

(defmacro tabbed-event-list (name base-class number)
  "If the event-list is in a tab page, use this to make sure you stay on the correct
tab when navigating through it"
  `(progn
     (defcomponent ,name (,base-class)
       ((tabbed-view :accessor tabbed-view
		     :initarg :tabbed-view
			      :initform ())))
     (defaction do-filter :after ((page ,name) filter)
       (setf (current-tab (tabbed-view page)) ,number))
     (defaction next-items :after ((page ,name))
		(setf (current-tab (tabbed-view page)) ,number))
     (defaction prev-items :after ((page ,name))
		(setf (current-tab (tabbed-view page)) ,number))
     (defaction navigate-to :after ((page ,name) index)
		(setf (current-tab (tabbed-view page)) ,number))
     (defaction navigate-to-page :after ((comp ,name) page-number)
		(setf (current-tab (tabbed-view page)) ,number))
     (defaction do-filter :after ((page ,name) filter)
		(setf (current-tab (tabbed-view page)) ,number))
     (defaction do-results-per-page :after ((page ,name) count)
		(setf (current-tab (tabbed-view page)) ,number))))

(defaction show-event-timeline-graph ((page paragent-component) user &key height width)
  (call-component nil (make-instance 'event-timeline-graph :user user :height height :width width)))


(defmacro <event-timeline-graph (page user &key (height 100) (width 250))
  `(<:img :src (make-link (show-event-timeline-graph ,page ,user :height ,height :width ,width))))

(defcomponent event-timeline-graph (img-line-graph)
  ((user :accessor user
         :initarg :user
         :type user))
  (:documentation "Renders a line graph showing the various event types over the past week"))

(defmethod render :before ((img event-timeline-graph))
  "We do this here rather than init-instance to avoid certain ucw issues"
  (let* ((today (get-date))
         (duration 7))
    (setf (x-labels img)
          (loop for i from duration downto 1 collecting
                (print-dow (clsql-sys::date->time
                             (date- today (make-duration :day i))) :short)))
    (setf (data img)
          (with-db
            (start-sql-recording)
            (mapcar
              (lambda (id.color)
                (list
                  (cdr id.color)
                  (loop for i from duration downto 0 collecting
                        (car
                          (select
                            [count [slot-value 'event 'id]]
                            :from [events]
                            :refresh t :flatp t
                            :where [and
                            [= [slot-value 'event 'company-id] (company-id (user img))]
                            [<= [slot-value 'event 'severity-id] (car id.color)]
                            [> [timestamp] (date- today (make-duration :day i))]
                            [<= [timestamp]
                            (if (<= i 1)
                                (date+
                                  today
                                  (make-duration :day 1))
                                (date-
                                  today
                                  (make-duration :day (- i 1))))]])))))
              `((0 . ,(chart::get-color :note))
                (6 . ,(chart::get-color :low))
                (8 . ,(chart::get-color :medium))
                (10 . ,(chart::get-color :high))))))))

#.(clsql:restore-sql-reader-syntax-state)
