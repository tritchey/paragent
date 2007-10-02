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


(defaction goto-inventory-report-page ((page paragent-component))
  (call 'inventory-report-page :user (user page)))

;;; handy function for grabbing the total memory from a computer

(defgeneric total-memory (computer))

(defmethod total-memory ((computer computer))
  (list (readable-byte-size (reduce 
		       (lambda (&optional a b)
			 (if (and a b)
			     (+ a b)
			     (or a b)))
		       (mapcar #'capacity (memory computer))))))

(defparameter *comp-props*
  '(("IP Address" ("IP Address" ip-addresses))
    ("Bios" ("Bios" bios))
    ("CD-Roms" ("CD-Roms" cd-roms))
    ("Physical Drives" ("Physical Drives" hard-drives))
    ("Hard Drives" ("Hard Drives" logical-drives))
    ("Memory" ("Memory Total" total-memory) ("Memory Slots" db::memory-array) ("Memory" memory))
    ("Network Cards" ("Network Cards" network-cards))
    ("Operating System" ("Operating System" operating-system))
    ("Printers" ("Printers" printers))
    ("Processors" ("Processors" processors))
    ("Sound Devices" ("Sound Devices" sound-devices))
    ("Motherboard" ("Motherboard" motherboards))
    ("Video Controllers" ("Video Controllers" video-controllers))))

(defcomponent inventory-report-page (paragent-report-component)
  ((inventory-selector :accessor inventory-selector
                       :type inventory-selector))
  (:default-initargs 
   :title "Inventory"))


(defmethod initialize-instance :after ((page inventory-report-page) &key)
  (setf (inventory-selector page)
        (make-instance 'inventory-selector :user (user page)
                       :place (make-place (inventory-selector page)))))

(defaction do-nothing ((page paragent-component)))

(defmethod render-tasks ((page inventory-report-page))
  (<:h1 "Tasks")
  (<:ul
   (<:li (link-to-prefs "Manage User Accounts"))
   (<:li (link-to-alerts "Manage Alert Settings"))))

(defrender ((page inventory-report-page))
  (<:script :src "sorttable.js" :type "text/javascript")
  (let* ((user (user page))
         (inventory-selector (inventory-selector page))
         (selected-props (selected-props inventory-selector)))
    (show-tip user "From the inventory report, you can see what hardware you have on your network.")
    (render inventory-selector)
    
    (dolist (prop-checkbox (prop-checkboxes inventory-selector))
      (when (value (cdr prop-checkbox))
        
        (<:as-html "true" prop-checkbox)))
    (when (ready inventory-selector)
      (let* ((tag (value (tag-selector inventory-selector)))
             (computers (if tag 
			    (computers-for-tag tag user) 
			    (sort (copy-list (computers (company user))) #'name<))))
        (link-to-csv 
	 page 
	 (cons
	  (cons "Computer" (mapcar (lambda (header) (first header)) selected-props))
	  (mapcar
	   (lambda (computer)
	     (cons (name computer)
		   (mapcar
		    (lambda (prop)
		      (apply 
		       #'concatenate 
		       'string
		       (mapcar
			(lambda (val)
			  (render-prop-list
			   val
			   :display-func
			   (lambda (obj labels funcs)
			     (apply 
			      #'concatenate 
			      'string
			      (mapcar
			       (lambda (label func)
				 (if label
				     (format nil "~a: ~a~%" 
					     label (funcall func obj))
				     (format nil "~a~%" 
					     (funcall func obj))))
					   labels funcs)))))
			      (funcall (second prop) computer))))
		    selected-props)))
	   computers))
	 "Export to csv" :link-id "inventory-csv")
        (<:div :class "report" :id "inventory-report"
               (<:table
		:class "sortable" :id "inventory-table"
		:cellspacing 0 :cellpadding 0
		(<:tr
		 (<:th (<:as-html "Computer"))
		 (let ((column-bg t))
		   (dolist (header selected-props)
		     (<:th :class (if column-bg "shaded" "clear")
			   (<:as-html (first header)))
		     (setf column-bg (not column-bg)))))
		(dolist (computer computers)
		  (<:tr
		   (<:td
		    (<ucw:a :action (goto-computers-page page (user page) :computer computer)
			    (<:as-html (name computer))))
		   (let ((column-bg t))
		     (dolist (prop selected-props)
		       (let ((vals (funcall (second prop) computer)))
			 (<:td :class (if column-bg "shaded" "clear")
			       (if vals
				   (let ((len (length vals))
					 (i 0))
				     (dolist (val vals)
				       (<:p (render-prop-list val))
				       (incf i)
				       (unless (equal i len)
					 (<:hr :class "inventory-line"))))
				   (<:as-html "None")))
			 (setf column-bg (not column-bg)))))))))))))


(defcomponent inventory-selector (paragent-component)
  ((ready :accessor ready
          :type boolean
          :initform nil)
   (tag-selector :accessor tag-selector)
   (prop-checkboxes :accessor prop-checkboxes
                    :type list))
  (:documentation "Allows the user to select which properties and computers they want to see."))

(defgeneric selected-props (page))

(defmethod selected-props ((page inventory-selector))
  (let ((ret nil))
    (dolist (prop-checkbox (prop-checkboxes page))
      (when (value (cdr prop-checkbox))
        (setf ret (append (cdr (assoc (car prop-checkbox) *comp-props* :test #'string=)) ret))))
    ret))


(defmethod initialize-instance :after ((page inventory-selector) &key)
  (with-db
    (setf (prop-checkboxes page)
          (mapcar
            (lambda (prop)
              (cons (first prop)
                    (make-instance 'checkbox-field)))
            *comp-props*))
    (setf (tag-selector page)
          (make-instance 'alist-select-field
                         :data-set
                         (cons (cons "All" nil)
                               (mapcar
                                 (lambda (tag)
                                   (cons tag tag))
                                 (get-tags (company-id (user page)))))))))

(defaction filter-inventory ((page inventory-selector))
  (setf (ready page) t))

(defmethod render ((page inventory-selector))
  (<:h1 :class "report" "Inventory Report")
  (<:div :class "report-form" :id "inventory-report-form"
         (<ucw:form
           :action (filter-inventory page)
           (render (tag-selector page))
           (columnize
             (lambda (name+checkbox)
               (let ((name (car name+checkbox))
                     (checkbox (cdr name+checkbox)))
                 (<:label :for (dom-id checkbox) (render checkbox) (<:as-html name))))
             (prop-checkboxes page)
             :num-columns 4
             )
           (<ucw:input :action (filter-inventory page)
                       :class "ok"
                       :type "image"
			:src "images/genrepbtn.gif"))))


