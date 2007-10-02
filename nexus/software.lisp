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

(defgeneric computer-count (software)
  (:documentation "Returns the number of computers with this software installed"))

(defmethod computer-count ((license-count license-count))
  (caar
    (query (format nil "select count(*) from software_computer_link where software_computer_link.SOFTWARE_ID = ~a;"
                   (software-id license-count)))))

(defmethod computer-count ((software software))
  (caar
    (query (format nil "select count(*) from software_computer_link where software_computer_link.SOFTWARE_ID = ~a;"
                   (id software)))))


(defun marshall-to-software (prop-list)
  (declare (list prop-list))
  (mapcar
    (lambda (prop-list)
      (let ((id (first prop-list))
            (name (second prop-list))
            (publisher (third prop-list))
            (company-id (fourth prop-list)))
        (make-instance 'software :id id :name name :publisher publisher :company-id company-id
                       :view-database *default-database*)))
    prop-list))

(defun select-software (&rest args)
  "Wrapper around a regular select statement to stuff all of our queried properties back into software objects.
Necessary because the oo select doesn't handle :from properly in my advanced queries"
  (marshall-to-software
    (apply #'select
           [slot-value 'software 'id] [slot-value 'software 'name] [slot-value 'software 'publisher]
           [slot-value 'software 'company-id]
           args)))

(defun get-unlicensed-software (user &key scheme)
  "Uses some fancy sql to select only that software which exceeds its license count. Way better than our old way, which required many, many queries"
  (declare (user user))
  (unless scheme (setf scheme (get-software-scheme user)))
  (let ((exceeded-software (select-software
                             :from (list [software])
                             :inner-join [license-count]
                             :on [= [slot-value 'license-count 'software-id] [slot-value 'software 'id]]
                             :where [and
                             [= [slot-value 'software 'company-id] (company-id user)]
                             [= [slot-value 'license-count 'license-scheme-id] (id scheme)]
                             [>
                             [select [count [slot-value 'software-computer-link 'id]] :from [software-computer-link]
                             :where [= [slot-value 'software-computer-link 'software-id] [slot-value 'software 'id]]]
                             [slot-value 'license-count 'num]]]))
        (exceeded-licenses nil)
        (installed-on nil))
    (dolist (software exceeded-software)
      (let ((license-count (license-count software scheme))
            (computers (computers software)))
        (if (and license-count (> (length computers)
                                  (num license-count)))
            (progn
              (push license-count exceeded-licenses)
              (push computers installed-on)))))
    (values exceeded-software exceeded-licenses installed-on)))


(defun software-audit-p (user &key scheme)
  "Returns t if the software audit passes"
  (declare (user user))
  (unless scheme (setf scheme (get-software-scheme user)))
  (not (select
         [slot-value 'software 'id]
         :from (list [software])
         :limit 1
         :inner-join [license-count]
         :on [= [slot-value 'license-count 'software-id] [slot-value 'software 'id]]
         :where [and
         [= [slot-value 'software 'company-id] 1]
         [= [slot-value 'license-count 'license-scheme-id] (id scheme)]
         [>
         [select [count [slot-value 'software-computer-link 'id]] :from [software-computer-link]
         :where [= [slot-value 'software-computer-link 'software-id] [slot-value 'software 'id]]]
         [slot-value 'license-count 'num]]])))


(defun get-software-scheme (user)
  (declare (user user))
  (car (select 'license-scheme :flatp t :limit 1 :where [= [company-id] (company-id user)])))



;;; Software list

(defcomponent software-list (filtered-item-list)
  ((scheme :accessor scheme
           :type license-scheme
           :initarg :scheme)
   (filter :accessor filter
           :initarg :filter
           :initform ""))
  (:documentation "Displays the software in the company")
  (:default-initargs
    :div-id "all-software"
    :results-per-page 30
    :cellspacing 0
    :use-optimized t
    :default-view-classes (list [software])))

(defmethod render-title ((page software-list))
  (<:div
    :class "box-title"
    (<:h2 (<:as-html "Software"))
    (<:ul :class "filter-list"
     (<:li :class "export first-item"
	   (link-to-csv page
			(cons (list "Software" "Publisher" "Installations" "Licenses")
			      (export-list-getter page))
			"Export to csv")))))

(defmethod render-header ((page software-list))
  )

 
(defmethod export-list-getter ((page software-list)) 
  (with-db
    (query
      (format nil "select software.NAME, software.PUBLISHER, (select count(software_computer_link.ID) from software_computer_link
              where software_computer_link.SOFTWARE_ID = software.ID), license_count.NUM
from software
left outer join license_count on license_count.SOFTWARE_ID = software.ID
  and license_count.LICENSE_SCHEME_ID = ~a
where software.COMPANY_ID = ~a and ~a
order by  (select count(software_computer_link.ID) from software_computer_link
              where software_computer_link.SOFTWARE_ID = software.ID)
           - license_count.NUM DESC, software.NAME;"
              (id (scheme page))
              (company-id (user page))
              (sql (get-filter-sql page))))))


(defmethod optimized-list-getter ((page software-list) start len)
  (marshall-to-software 
    (query 
      (format nil "select software.ID, software.NAME, software.PUBLISHER, software.COMPANY_ID
from software
left outer join license_count on license_count.SOFTWARE_ID = software.ID
  and license_count.LICENSE_SCHEME_ID = ~a
where software.COMPANY_ID = ~a and ~a
order by  (select count(software_computer_link.ID) from software_computer_link
              where software_computer_link.SOFTWARE_ID = software.ID)
           - license_count.NUM DESC, software.NAME
LIMIT ~a OFFSET ~a;"
              (id (scheme page))
              (company-id (user page))
              (sql (get-filter-sql page))
              len start))))


(defmethod optimized-list-count ((page software-list))
  (caar
    (query 
      (format nil "select count(software.ID)
from software
left outer join license_count on license_count.SOFTWARE_ID = software.ID
  and license_count.LICENSE_SCHEME_ID = ~a
where software.COMPANY_ID = ~a and ~a;"
              (id (scheme page))
              (company-id (user page))
              (sql (get-filter-sql page))))))

(defmethod render-item ((page software-list) software)
  (declare (type software software))
  (let* ((allowed-num (license-count software (scheme page)))
         (computers (computers software))
         (num-installs (length computers)))
    
      (render-collapsing-box2 t (if (>= num-installs 100) 0 (+ 0.3 (float (/ num-installs 100))))
        (<:tr
         :class (if (odd-row page) "odd-row" "even-row")
         (<:td (cond ((not allowed-num) 
		      "")
		     ((< (num allowed-num) num-installs) 
		      (<:img :src "images/cond-High.gif"))
		     (t 
		      (<:img :src "images/okay.gif"))))
         (if (not allowed-num) (setf allowed-num
                                      (make-instance 'license-count
                                                     :software-id (id software)
                                                     :num ""
                                                     :license-scheme-id (id (scheme page)))))
          (<:td (collapsing-link (<:ah " " (name software)))
                (collapsing-body
                  <:div
                  (<:ul :class "software-computer-list"
                        (dolist (computer computers)
                          (let ((computer computer))
                            (<:li (<:a :href (computer-link computer)
                                       (<:as-html (name computer)))))))))
          
          
          (let ((save-id (unique-id "save"))
                (blank-id (unique-id "blank")))
            (<ucw:form
              :action (save-license-count page allowed-num)
              (<:td :align "right" :valign "top"
                    (<:span
                      :class "count"
                      (<:as-html num-installs " / ")
                      (let ((jscript (format nil "Effect.Appear('~a');" save-id)))
				     ;(js:js-to-string
				     ;  `(progn (.-appear -effect ,save-id)))))
                        (<ucw:text :accessor (num allowed-num) :size 5
                                   :onkeypress jscript
                                   :onchange jscript))
                      (<:script
                        (<:as-is "this.form.onsubmit = function() { flashMessage('Saving...');};"))))
              (<:td :valign "top" :width "30px"
		    :class "savebtn"
                    (<ucw:input
                      :id save-id
                      :style "display: none;"
                      :type "image"
                      :src "images/save.gif"
                      :action (save-license-count page allowed-num)
                      :value "Save")
		    (<:ai "&nbsp;")
                    
                    (when (<= num-installs 0)
                      (<ucw:a :action (delete-software page software) 
                              (<:img :src "images/trash.png" :alt "delete" :title "delete")))
                    )))))))

(defaction delete-software ((page paragent-component) software)
  (delete-software% software))

(defun delete-software% (software)
  (declare (software software))
  (with-db
   (delete-records :from [license-count]
                   :where [= [software-id] (id software)])
   (delete-instance-records software)))

(defmethod initialize-instance :after ((page software-list) &key)
  (unless (slot-boundp page 'scheme)
      (setf (scheme page)
            (with-db
              (get-software-scheme (user page))))))



;;; Software page

(defaction goto-software-page ((page simple-window-component) user)
  (call 'software-page :user user))

(defcomponent software-page (paragent-window-component)
  ((scheme :accessor scheme
           :type license-scheme
           :initarg :scheme)
   (software-list :accessor software-list
                  :type software-list))
  (:default-initargs 
   :title "Software Audit"))

(defmethod body-id ((component software-page))
  "software")

(defmethod initialize-instance :after ((page software-page) &key)
  (setf (scheme page)
        (with-db
          (get-software-scheme (user page))))
  (setf (software-list page)
        (make-instance 'software-list :user (user page))))

(defrender ((page software-page))
  (show-tip (user page)
            "This page is used to keep track of your compliance with your software licenses. 
Simply enter the number of licenses you have in the box on the right and click the \"Save\" button. 
Any exceeded licenses will be displayed first.")
  (with-db
    (render (software-list page))))

(defmethod render-tasks ((page software-page))
  (<:h1 "Tasks")
  (<:ul
    (<:li (link-to-license-keys-report "View License Keys"))
    (<:li (link-to-software-report "View Summary"))))


(defun save-license-count% (page license-count)
  (declare (ignore page))
  (with-db
    (if (equal (num license-count) "")
        (progn
          (when (clsql-sys::view-database license-count)
            (clsql:delete-instance-records license-count))
          (setf (num license-count) "cake"))
        (let ((num (parse-integer (num license-count) :junk-allowed t)))
          (cond
            ((and num (>= num 0))
             (progn
               (setf (num license-count) num)
               (update-records-from-instance license-count)))
            (t
              (setf (num license-count) "")))))
    ))
  
(defaction save-license-count ((page software-list) license-count)
  (save-license-count% page license-count))



#.(clsql:restore-sql-reader-syntax-state)
