#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#
;;;; This file holds various handy functions that don't fit in any specific place

(in-package :nexus)

#.(clsql:locally-enable-sql-reader-syntax)


(defmethod name ((computer computer))
  "This exists to easily add the ability to give computers custom names.
This way, we redirect all of Nexus's name queries to the alias slot without affecting Archon."
  (slot-value computer 'alias))

(defmethod unaliased-name ((computer computer))
  "Returns the computer's actual name, according to the OS"
  (slot-value computer 'name))



(defun link-for-hotfix (hotfix)
  "Pulls the hotfix number out of our description string and returns the link to the Microsoft KB article"
  (declare (string hotfix))
  (let ((ret (cl-ppcre:register-groups-bind 
               (hotfix-number) (".*'[a-zA-Z]*(.*)'.*" hotfix)
               (format nil "http://support.microsoft.com/kb/~a" hotfix-number))))
    (or ret
         (cl-ppcre:register-groups-bind
           (hotfix-number2) ("[a-zA-Z]*(.*)" hotfix)
           (format nil "http://support.microsoft.com/kb/~a" hotfix-number2)))))


;;; TODO: figure out a way to change our key based on where we're running this
(defun import-google-search-api ()
  "Figures out the proper key to use to load Google's Search Api"
  (when *google-search-enabled-p*
    (<:script 
      :type "text/javascript"
      :src (format nil "https://www.google.com/uds/api?file=uds.js&amp;v=1.0&amp;key=~a"
                   (cond
                     (*server-is-archon*
                       "ABQIAAAANX8mCgBfUCwu0cYr8mTwVBTfT6u28-o6SNKe1zTF09qK2Ai32hT8gKv3ZCBsdTFlr37c-LU1IYCJoA") ; archon
                     ((stringp *google-key*) *google-key*) ; for appliances
                     ((equal "127.0.0.1" (ucw::peer-address (context.request *context*)))
                      "ABQIAAAANX8mCgBfUCwu0cYr8mTwVBT2yXp_ZAY8_ufC3CFXhHIE1NvwkxT2_SwZUxNKQREhCgfNAL8UOpjqew") ; localhost
                     (t "ABQIAAAANX8mCgBfUCwu0cYr8mTwVBTAD6MD7e-gRC4OETpupdTgDaR7RhSfh1hgNJdPoPOM4ETB6Wfk2oeWdA") ; aldaris
                     )))))


(defmacro make-link (action)
  "Returns the url that maps to given action"
  `(action-href
     (lambda ()
       (arnesi:with-call/cc
         ,action))))


(defmacro db-count (table &rest args)
  "Shortcut to count the number of items that fit a given query"
  `(car (select [count [id]] :refresh t :flatp t :from ,table ,@args)))


(defmacro db-chunk ((var query) &body body)
  "Pulls in database results in limited size chunks to work on,
allowing us to avoid heap exhaustion"
  (let ((limit 2000)
	(offset (gensym))
	(items (gensym)))
    `(let* ((,items '(1)))
       (declare (list ,items))
       (loop for ,offset = 0 then (+ ,offset ,limit)
	     for i from 1 to 3
	     until (null ,items) do
	     (setf ,items (,@query :offset ,offset :limit ,limit))
	     (dolist (,var ,items)
	       ,@body)
	     (when ,items (setf ,items '(1))) ;turns our data into garbage
	     (sb-ext:gc :full t) ;make sure we clean up the old data
	     ))))


;;; Group & permissions stuff

(defun default-group-p (group)
  "Tells if this group is the special default group which applies to all users"
  (declare (group group))
  (equal (name group) "Default"))

(defmacro def-has-permission (name slot)
  "Defines a function to determine if the user has a given permission on the computer"
  `(defun ,name (user &optional computer)
     (declare (user user))
     (or (>=admin user)
         (if computer
             (select [slot-value 'group 'name]
                     :distinct t :limit 1
                     :from (list [groups] [group-user-links] [group-computer-links])
                     :where [or
                     [and
                     [= [slot-value 'group 'company-id] (company-id user)]
                     [= [slot-value 'group ',slot] 1]
                     [= [slot-value 'group 'name] "Default"]]
                     
                     [and
                     [= [slot-value 'group 'company-id] (company-id user)]
                     [= [slot-value 'group ',slot] 1]
                     
                     [= [slot-value 'group 'id] [slot-value 'group-user-link 'group-id]]
                     [= [slot-value 'group-user-link 'user-id] (id user)]
                     
                     [or
                     [= [slot-value 'group 'all-computers] 1]
                     
                     [and
                     [= [slot-value 'group 'id] [slot-value 'group-computer-link 'group-id]]
                     [= [slot-value 'group-computer-link 'computer-id] (id computer)]
                     ]]
                     ]])
             (select [slot-value 'group 'name]
                     :distinct t :limit 1
                     :from (list [groups] [group-user-links])
                     :where [or
                     [and
                     [= [slot-value 'group 'company-id] (company-id user)]
                     [= [slot-value 'group ',slot] 1]
                     [= [slot-value 'group 'name] "Default"]]
                     
                     [and
                     [= [slot-value 'group 'company-id] (company-id user)]
                     [= [slot-value 'group ',slot] 1]
                     
                     [= [slot-value 'group 'id] [slot-value 'group-user-link 'group-id]]
                     [= [slot-value 'group-user-link 'user-id] (id user)]
                     ]])))))

;(def-has-permission has-remote-permission remote-permission)
(def-has-permission has-shutdown-permission shutdown-permission)
(def-has-permission has-note-permission note-permission)

(defun has-remote-permission (user &optional computer)
  "This permission has to be defined differently, because permission also depends on your account type"
  (declare (user user))
  (and (or (trial-enabled-p (company user))
	   (>=account-premium (company user)))
       (or (>=admin user)
           (if computer
               (select [slot-value 'group 'name]
                       :distinct t :limit 1
                       :from (list [groups] [group-user-links] [group-computer-links])
                       :where [or
                       [and
                       [= [slot-value 'group 'company-id] (company-id user)]
                       [= [slot-value 'group 'remote-permission] 1]
                       [= [slot-value 'group 'name] "Default"]]
                       
                       [and
                       [= [slot-value 'group 'company-id] (company-id user)]
                       [= [slot-value 'group 'remote-permission] 1]
                       
                       [= [slot-value 'group 'id] [slot-value 'group-user-link 'group-id]]
                       [= [slot-value 'group-user-link 'user-id] (id user)]
                       
                       [or
                       [= [slot-value 'group 'all-computers] 1]
                       
                       [and
                       [= [slot-value 'group 'id] [slot-value 'group-computer-link 'group-id]]
                       [= [slot-value 'group-computer-link 'computer-id] (id computer)]
                       ]]
                       ]])
               (select [slot-value 'group 'name]
                       :distinct t :limit 1
                       :from (list [groups] [group-user-links])
                       :where [or
                       [and
                       [= [slot-value 'group 'company-id] (company-id user)]
                       [= [slot-value 'group 'remote-permission] 1]
                       [= [slot-value 'group 'name] "Default"]]
                       
                       [and
                       [= [slot-value 'group 'company-id] (company-id user)]
                       [= [slot-value 'group 'remote-permission] 1]
                       
                       [= [slot-value 'group 'id] [slot-value 'group-user-link 'group-id]]
                       [= [slot-value 'group-user-link 'user-id] (id user)]
                       ]])))))




;;; cookie handling

(defun get-cookie (name)
  "Returns the value of the cookie with the given name"
  (declare (string name))
  (remove-if (lambda (c) (equal c #\"))
             (ucw::context.cookie-value ucw:*context* name)))

(defun set-cookie (name value &optional (lifespan (* 60 60 24 7 2)))
  "Sets the value of the cookie with the given name. 2 weeks is the default lifespan."
  (declare (string name value))
  (ucw::add-header (context.response *context*) "Set-Cookie"
                   (rfc2109:cookie-string name value :path "/" :max-age lifespan)))

(defun internet-explorer-p ()
  "Returns t if the user is running IE"
  (search
    "MSIE"
    (get-header (context.request *context*) "User-Agent")))

(defun get-user ()
  "Returns the user and their company, if one with the proper login info is found"
  (with-db
    (let* ((username (get-cookie "user"))
           (session-id (get-cookie "session-id"))
           (result (car (select [slot-value 'user 'id] 
				[slot-value 'user 'level]
                                [slot-value 'user 'company-id] 
				[slot-value 'user 'name]
                                [slot-value 'user 'email] 
				[slot-value 'user 'username]
                                [slot-value 'user 'password] 
				[slot-value 'user 'last-login]
				[slot-value 'user 'timezone-preference]
                                [slot-value 'user 'weekly-software-report] 
				[slot-value 'user 'recent-computers]
                                
                                [slot-value 'company 'disabled]
                                [slot-value 'company 'id] [slot-value 'company 'name]
                                [slot-value 'company 'secret] [slot-value 'company 'msi]
                                [slot-value 'company 'level] [slot-value 'company 'created]
                                
                                [slot-value 'user-session 'expiration] 
                                
                                :from (list [users] [companies] [user-sessions])
                                :where [and [= [slot-value 'user 'username] username]
                                [= [slot-value 'user-session 'session-id] session-id]
                                [= [slot-value 'user-session 'user-id] [slot-value 'user 'id]]
                                [= [slot-value 'user 'company-id] [slot-value 'company 'id]]]
                                :limit 1 :flatp t))))
      (when result
	(let* ((company (make-instance 'company  
				       :disabled (clsql-sys:read-sql-value 
						  (nth 11 result) 
						  'boolean 
						  *default-database* 
						  (database-type *default-database*))
				       :id (nth 12 result) 
				       :name (nth 13 result)
				       :secret (nth 14 result) 
				       :msi (nth 15 result)
				       :level (nth 16 result)
				       :created (clsql-sys:read-sql-value 
						 (nth 17 result) 
						 'date 
						 *default-database* 
						 (database-type *default-database*))
				       :view-database *default-database*))
	       (user (make-instance 'user 
				    :id (first result) 
				    :level (second result) 
				    :company-id (third result)
				    :name (fourth result) 
				    :email (fifth result) 
				    :username (sixth result)
				    :password (seventh result) 
				    :last-login (clsql-sys:read-sql-value 
						 (eighth result) 
						 'wall-time 
						 *default-database* 
						 (database-type *default-database*))
				    :timezone-preference (ninth result)
				    :weekly-software-report (clsql-sys:read-sql-value 
							     (tenth result) 
							     'boolean 
							     *default-database* 
							     (database-type *default-database*))
				    :recent-computers (when (tenth result)
							(clsql-sys:read-sql-value 
							 (nth 10 result) 
							 T 
							 *default-database* 
							 (database-type *default-database*)))
				    :company company
				    :view-database *default-database*))
	       (expiration (clsql-sys:read-sql-value 
	                    (nth 18 result)
	                    'wall-time
	                    *default-database*
	                    (database-type *default-database*))))
	  (when (and user company)
	    (if (time> expiration (get-time))
	        (progn
	          (values user company))
	        (progn
	          (clsql:delete-records :from [user-sessions]
	                                :where [<= [expiration] (get-time)])
	          nil))))))))


(defun get-guest ()
  "Returns the guest account"
  (with-db
    (car (select 'user :where [= [username] "guest"] :flatp t :limit 1))))

(defun guest-p (user)
  "Tells if this user is the guest account"
  (declare (type user user))
  (equal (name user) "guest"))

(defmacro render-tip-box (&rest rest)
  "Displays the body in a tip box"
  (let ((id (unique-id "tip")))
    `(<:div :class "tip" :id ,id
            (<:a :class "close-button" :href "#"
                 :onclick ,(format nil "javascript:Effect.Fade($('~a')); return false;" id)
                 (<:img :src "images/close.gif"))
            ,@rest)))

(defmacro show-tip (user &rest rest)
  "Displays this tip to the guest account"
  `(when (guest-p ,user)
     (render-tip-box ,@rest)))


(defun unique-id (&optional (prefix "paragent"))
  "Returns a unique id, suitable for using as an html element's id.
An optional prefix is provided to make the id more descriptive"
  (js:gen-js-name-string :prefix prefix))

(defun car-name= (a b)
  "Helper function to compare the names of objects which are wrapped in a list"
  (name= (car a) (car b)))

(defun timestamp< (a b)
  "sort objects by timestamp"
  (not (time>= (timestamp a) (timestamp b))))

(defun name< (a b)
  "Helper function to sort objects by name"
  (string-lessp (name a) (name b)))

(defun car-name< (a b)
  "Helper function to sort objects by name when those objects are wrapped in a list"
  (name< (car a) (car b)))

;; User levels

(defun >=admin (user)
  "Returns t if the user has admin access or greater.
Admins can do everything. They're so cool!"
  (declare (user user))
  (>= (level user) 10))

(defun >=technician (user)
  "DEPRECATED: We now use groups for this sort of thing.
Returns t if the user has technician access or greater.
Technicians can do everything but create new users."
  (declare (user user))
  (>= (level user) 5))

(defun string-replace (str1 sub1 sub2)
  "Handy function to replace all instances of a given substring with a new string"
  (declare (string str1 sub1 sub2))
  (if (string= str1 "")
      ""
      (let ((str1 (string str1))
            (str2 "")
            (sub1 (string sub1))
            (sub2 (string sub2))
            (index1 0))
        (loop
          (if (string-equal str1 sub1
                            :start1 index1
                            :end1 (min (length str1)
                                       (+ index1 (length sub1))))
              (progn
                (setq str2 (concatenate 'string str2 sub2))
                (incf index1 (length sub1)))
              (progn
                (setq str2 (concatenate 'string
                                        str2
                                        (subseq str1 index1 (1+ index1))))
                (incf index1)))
          (unless (< index1 (length str1))
            (return str2))))))

(defun not-blank (string)
  "Returns t if a string is null or empty"
  (declare ((or null string) string))
  (and string (not (equal string ""))))

(defmacro use-if (value alternate-value)
  "Macro I made before I realized that 'and' does the same thing"
  (let ((var (gensym)))
    `(let ((,var ,value))
       (if ,var
           ,var
           ,alternate-value))))

(defmacro left-right-align (left right)
  "Cheap way of aligning items to the left and right. Wraps a table around them"
  `(<:table
     :width "100%" :border 0 :class "aligner"
     (<:tr :class "aligner"
	   (<:td :align "left" :class "aligner" ,left)
	   (<:td :align "right" :class "aligner" ,right))))


(defmacro left-center-right-align (left center right)
  "Cheap way of aligning items to the left and right and center. Wraps a table around them"
  `(<:table
     :width "100%" :border 0 :class "aligner"
     (<:tr :class "aligner"
	   (<:td :align "left" :width "33%" :class "aligner" ,left)
           (<:td :align "center" :width "33%" :class "aligner" ,center)
	   (<:td :align "right" :width "33%" :class "aligner" ,right))))


(defmacro tableify (&rest cells)
  "Handy macro to throw everything into some table cells"
  `(<:table
     :border 0 :cellspacing 5 :class "aligner"
     ,@(mapcar
         (lambda (label-field)
           `(<:tr
	      :class "aligner"
              (<:td :class "aligner" ,(first label-field))
              (<:td :class "aligner" ,@(cdr label-field))))
         cells)))

(defun columnize (renderer items &key (num-columns 5))
  "Renders items into the specified number of columns"
  (declare (function renderer))
  (declare (list items))
  (<:table
    :border 0 :cellspacing 5 :width "100%"
    (loop for i from 0 to (1- (length items)) do
          (when (zerop (mod i num-columns))
            (<:tr
              (loop for i2 from i to (1- (+ i num-columns)) do
                    (let ((item (nth i2 items)))
                      (<:td 
                        (if item 
                            (funcall renderer item)
                            (<:ai "&nbsp;"))))))))))


(defmacro make-into-dl (&key class list)
  "Handy macro to make everything into a dl"
  `(<:dl :class ,class
         ,@(mapcar
             (lambda (item)
               `(progn
                  (<:dt ,(first item))
                  (<:dd ,@(cdr item))))
             list)))

(defgeneric get-tags (company)
  (:documentation "Returns all the tags that are defined within a company"))

(defmethod get-tags ((company company))
  "Returns all the tags that are defined within a company"
  (get-tags (id company)))

(defmethod get-tags ((company-id integer))
  "Returns all the tags that are defined for a company with the given id"
  (select [slot-value 'computer-tag 'name] :refresh t :flatp t :distinct t
          :from (list [computer-tag] [computers])
          :order-by [slot-value 'computer-tag 'name]
          :where [and [= [slot-value 'computer-tag 'computer-id] [slot-value 'computer 'id]]
          [= [slot-value 'computer 'company-id] company-id]]))

(defgeneric computers-for-tag (tag company)
  (:documentation "Returns the computers with the given tag"))

(defmethod computers-for-tag (tag (user user))
  "Returns all the computers in the user's company which have a given tag"
  (computers-for-tag tag (company-id user)))

(defmethod computers-for-tag (tag (company company))
  "Returns all the computers in the company which have a given tag"
  (computers-for-tag tag (id company)))

(defmethod computers-for-tag ((tag computer-tag) (company-id integer))
  "Returns all the computers in the company which have a given tag"
  (computers-for-tag (name tag) company-id))

(defmethod computers-for-tag ((tag string) (company-id integer))
  "Returns all the computers in the company which have a given tag"
  (select 'computer :refresh t :flatp t
          :order-by [slot-value 'computer 'name]
          :where [and
          [= [slot-value 'computer 'company-id] company-id]
          [= [slot-value 'computer-tag 'computer-id] [slot-value 'computer 'id]]
          [like [slot-value 'computer-tag 'name] tag]]))


(defun company-for-name (name)
  (with-db (car (select 'company :flatp t :limit 1
                        :where [like [name] name]))))


(defun computer-for-id (computer-id company-id)
  (with-db (car (select 'computer :flatp t :limit 1
                        :where [and
                        [= [company-id] company-id]
                        [= [id] computer-id]]))))

(defun computers-by-guid (computers)
  (declare (list computers))
  (let ((comp-id-hash (make-hash-table)))
    (dolist (computer computers)
      (let ((id (id computer))
            (id-list (gethash (archon-connection computer) comp-id-hash)))
        (if id-list
            (push id id-list)
            (setf id-list (list id)))
        (setf (gethash (archon-connection computer) comp-id-hash) id-list)))
    comp-id-hash))

(defun computer-for-name (computer-name company-id)
  "Returns the computer with the given alias. If that doesn't exist, returns one with that name."
  (with-db (or (car (select 'computer :flatp t :limit 1
                            :where [and [= [company-id] company-id]
                            [= [alias] computer-name]]))
               (car (select 'computer :flatp t :limit 1
                            :where [and [= [company-id] company-id]
                            [= [name] computer-name]])))))


#.(clsql:restore-sql-reader-syntax-state)

