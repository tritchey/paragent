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


;;; Group list


(defcomponent group-list (vertical-tabbed-view)
  ((parent :accessor parent
           :initarg :parent)
   (groups :accessor groups
           :type list
           :initform '()))
  (:default-initargs
    :toolbar #'render-title))

(defmethod initialize-instance :after ((page group-list) &key)
  (with-db
   (let* ((user (user page))
          (groups (groups (company-id user))))
     (setf (groups page) groups)
     (setf (contents page)
           (mapcar #'(lambda (group)
                       #'(lambda (page) (render-item page group)))
                   groups))
     (initialize-ids page))))

(defmethod render :before ((page group-list))
  "We reset the tab titles each render in case the name of the item got changed"
  (setf (tabs page)
        (mapcar #'name (groups page))))

(defmethod render-title ((page group-list))
  (<:h2 "User groups in your company")
  (init-computer-selector (user page)))

(defmethod unoptimized-list-getter ((page group-list))
  (let ((user (user page)))
    (select 'group :flatp t
            :where [= [slot-value 'group 'company-id] (company-id user)])))

(defmethod render-item ((page group-list) group)
  (declare (type group group))
  (<:div
   (let ((form (make-instance 'group-form :group group :user (user page))))
     (<ucw:form
      :action (save-group form group) :id (dom-id form)
      (if (default-group-p group)
          (<:ah (name group))
          (<:p (<:label "Name: ") (render (name-field form))))
      (<:p (<:strong (<:ah (if (default-group-p group)
                               "Give everybody permission to:"
                               "Give this group permission to:"))))
      (<:p
       (<:label :for (dom-id (remote-field form))
                (render (remote-field form))
                " remote into computers"))
      (<:p
       (<:label :for (dom-id (shutdown-field form))
                (render (shutdown-field form))
                " shutdown/restart computers"))
      (<:p
       (<:label :for (dom-id (note-field form))
                (render (note-field form))
                " change notes and tags"))
      (if (default-group-p group)
          (progn
            (<:p (<:strong "for all computers."))
            (<:br))
          (progn
            (<:br)
            (<:p (<:strong (render (all-computers-field form))
                           (<:label :for (dom-id (all-computers-field form)) "for all computers")))
            (<:p (<:strong (render (specific-computers-field form))
                           (<:label :for (dom-id (specific-computers-field form))
                                    "for these computers:")))
            (render (computer-selector form))))
      (<:ul :class "action-list"
            (<:li :class "first-item"
                  (<:a :onclick (format nil "javascript:$('~a').submit(); return false;" (dom-id form))
                       :href "#" "save"))
            (unless (default-group-p group)
              (<:li
               (<ucw:a :action (delete-group page group)
                       :onclick "return confirm('Delete this group?');"
                       "delete"))))))))


(defcomponent group-form (paragent-component)
  ((dom-id :accessor dom-id
           :initform (unique-id "group-form")
           :type string)
   (group :accessor group
          :type group
          :initarg :group)
   (new-p :accessor new-p
          :initarg :new-p
          :type boolean
          :initform nil)
   (name-field :accessor name-field
               :type string-field)
   (remote-field :accessor remote-field
                 :type checkbox-field
                 :initform (make-instance 'checkbox-field))
   (shutdown-field :accessor shutdown-field
                   :type checkbox-field
                   :initform (make-instance 'checkbox-field))
   (note-field :accessor note-field
               :type checkbox-field
               :initform (make-instance 'checkbox-field))
   (all-computers-group :accessor all-computers-group
                        :type radio-group
                        :initform (make-instance 'radio-group))
   (all-computers-field :accessor all-computers-field
                        :type ucw::radio-button)
   (specific-computers-field :accessor specific-computers-field
                             :type ucw::radio-button)
   (computer-selector :accessor computer-selector
                      :type computer-selector)))

(defmethod initialize-instance :after ((form group-form) &rest rest)
  (declare (ignore rest))
  (with-db
    (let ((group (group form)))
      (let ((radio-group (all-computers-group form)))
        (setf (all-computers-field form) 
              (make-instance 'ucw::radio-button :group radio-group
                             :value t))
        (setf (specific-computers-field form)
              (make-instance 'ucw::radio-button :group radio-group
                             :value nil))
        (setf (ucw::value-widgets radio-group)
              (list (all-computers-field form) (specific-computers-field form)))
        (setf (value radio-group) (all-computers group)))
      (setf (name-field form)
            (unless (default-group-p group)
              (make-instance 'string-field
                             :validators (list (make-instance 'not-empty-validator))
                             :value (name group))))
      (setf (computer-selector form) (make-instance 'computer-selector
                                                    :selected (computers group) :user (user form)))
      (setf (value (remote-field form)) (remote-permission group))
      (setf (value (shutdown-field form)) (shutdown-permission group))
      (setf (value (note-field form)) (note-permission group)))))

(defmethod render ((form group-form))
  (let ((group (group form)))
    (<:div
      :class "group-form"
      (<ucw:form
        :action (save-group form group)
        (unless (default-group-p group)
          (<:p (<:label "Name: ") (render (name-field form))))
        (<:p (<:strong (<:ah (if (default-group-p group)
                                 "Give everybody permission to:"
                                 "Give this group permission to:"))))
        (<:p
          (<:label :for (dom-id (remote-field form))
            (render (remote-field form))
            " remote into computers"))
        (<:p
          (<:label :for (dom-id (shutdown-field form))
            (render (shutdown-field form))
            " shutdown/restart computers"))
        (<:p
          (<:label :for (dom-id (note-field form))
            (render (note-field form))
            " change notes and tags"))
        (if (default-group-p group)
            (progn
              (<:p (<:strong "for all computers."))
              (<:br))
            (progn
             (<:br)
             (<:p (<:strong (render (all-computers-field form))
                            (<:label :for (dom-id (all-computers-field form)) "for all computers")))
             (<:p (<:strong (render (specific-computers-field form))
                            (<:label :for (dom-id (specific-computers-field form))
                                     "for these computers:")))
              (render (computer-selector form))))
        (<ucw:input :action (save-group form group)
                    :type "image" :class "button"
                    :value "Save" :src "images/savebtn.gif")))))

(defaction delete-group ((page paragent-component) group)
  (delete-group% group)
  (call-component nil (make-instance 'preferences-page :user (user page))))

(defun delete-group% (group)
  (with-db
    (clsql:delete-instance-records group)))

(defaction save-group ((page group-form) group)
  (when (and (save-group% page group) (new-p page))
    (goto-dialog-confirm page "Group created." *url-prefs*)))

(defgeneric save-group% (form group))

(defmethod save-group% ((form group-form) group)
  (with-db
    (clsql:start-sql-recording)
    (when (validp form)
      (when (name-field form)
        (setf (name group) (value (name-field form))))
      (setf (remote-permission group) (value (remote-field form)))
      (setf (shutdown-permission group) (value (shutdown-field form)))
      (setf (note-permission group) (value (note-field form)))
      (setf (all-computers group) (value (all-computers-group form)))
      (insert-and-update group)
      (unless (all-computers group)
        (delete-records :from (table-for-object 'group-computer-link)
                        :where [= [slot-value 'group-computer-link 'group-id] (id group)])
        (dolist (comp-id (new-selection (computer-selector form)))
          (update-records-from-instance
            (make-instance 'group-computer-link :group-id (id group) :computer-id comp-id))))))
  t)



;;; New group page

(defaction new-group ((page paragent-component))
  (call 'new-group-page :user (user page)))

(defcomponent new-group-page (paragent-dialog-component)
  ((group-form :accessor group-form))
  (:default-initargs
    :title "New Group"))

(defmethod initialize-instance :after ((page new-group-page) &key)
  (let ((user (user page)))
    (setf (group-form page)
          (make-instance 'group-form :user user :new-p t
                         :group (make-instance 'group :company-id (company-id user)
                                               :name "")))))

(defmethod render ((page new-group-page))
  (with-db
    (<:script :src "prototype.js")
    (<:script :src "selector.js")
    (init-computer-selector (user page))
    (render (group-form page))))






#.(clsql:restore-sql-reader-syntax-state)
