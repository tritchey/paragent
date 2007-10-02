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

(defaction goto-logged-error-page ((page paragent-component))
  (let ((user (user page)))
    (when (>=admin user)
      (call-component nil (make-instance 'logged-error-page :user user)))))


(defcomponent logged-error-list (item-list)
  ()
  (:documentation "Displays any internal application errors we've logged for perusal by the user.")
  (:default-initargs
    :div-id "logged-errors"
    :use-optimized t))

(defmethod optimized-list-count ((page logged-error-list))
  (car
    (select [count [slot-value 'logged-error 'id]]
            :from [logged-errors]
            :flatp t
            :where [and
            [= [slot-value 'logged-error 'company-id] (company-id (user page))]])))

(defmethod optimized-list-getter ((page logged-error-list) start len)
  (select 'logged-error
          :flatp t
          :offset start :limit len
          :where [and
          [= [slot-value 'logged-error 'company-id] (company-id (user page))]]))

(defmethod render-title ((page logged-error-list))
  (<:div :class "box-title"
         (<:h3 "Errors")))


(defmethod render-item ((page logged-error-list) logged-error)
  (declare (type logged-error logged-error))
  (let ((odd-row (if (odd-row page) "odd-row" "even-row")))
    (<:tr
     :class odd-row
     (<:td :rowspan 3 :align "center" :class "badge"
		   ;; format the timestamp
		   (make-event-badge (timestamp logged-error)))
     (<:td
      (dolist (line (split-sequence:split-sequence #\Newline (body logged-error)))
        (<:p (<:as-html line)))))
    (<:tr :class odd-row (<:td (<:br)))
    (<:tr 
     :class odd-row
     (<:td (<:ul
            :class "filter-list"
            (<:li :class "first-item"
                  (<ucw:a :action (delete-logged-error page logged-error) "delete")))))
    
    (<:tr (<:td (<:br)))
    ))

(defaction delete-logged-error ((page paragent-component) logged-error)
  (delete-logged-error% logged-error))

(defun delete-logged-error% (logged-error)
  (with-db
   (delete-instance-records logged-error)))


(defcomponent logged-error-page (paragent-window-component)
  ((item-list :accessor item-list
              :type logged-error-list))
  (:default-initargs
    :title "Errors"))

(defmethod initialize-instance :after ((page logged-error-page) &key)
  (setf (item-list page) (make-instance 'logged-error-list :user (user page))))

(defrender ((page logged-error-page))
  (render (item-list page)))

#.(clsql:restore-sql-reader-syntax-state)
