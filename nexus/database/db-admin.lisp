#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

;; This is sort of a poor man's mysqladmin

(in-package :nexus)

#.(clsql:locally-enable-sql-reader-syntax)

(defentry-point "db-admin.ucw" (:application *my-app*)
  ()
  (call 'db-login))


(defcomponent db-login (simple-window-component)
  ())


(defmacro with-database (&body body)
  "Macro that wraps all of the database creation and destruction"
  (let ((connected-p (gensym)))
    `(let* ((,connected-p *default-database*)
            (*default-database* (if ,connected-p
				    ,connected-p
				    (connect (list "" (db page) (user page) (password page))
					     :database-type :mysql :pool nil
					     :if-exists :new :make-default nil))))
      (unwind-protect (progn ,@body)
	(unless ,connected-p
	  (clsql:disconnect :database clsql:*default-database*))))))

(defmethod render ((page db-login))
  (let ((db "")
	(user "")
	(password "")
        (user2 "")
        (password2 ""))
    (<ucw:form
      :action (open-database page db user password user2 password2)
      (tableify
	("Database: " (<ucw:text :accessor db))
        ("User: " (<ucw:text :accessor user2))
	("Password: " (<ucw:text :accessor password2))
	("User: " (<ucw:text :accessor user))
	("Password: " (<ucw:text :accessor password)))
      (<ucw:submit :action (open-database page db user password user2 password2)))))

(defaction open-database ((page component) db user password user2 password2)
  (when (and (equal user2 "Overmind")
             (equal password2 "2Rav7qAD"))
    (call 'db-viewer :db db :user user :password password)))


(defcomponent db-page (simple-window-component)
  ((db :accessor db
       :initarg :db
       :type string)
   (user :accessor user
	 :initarg :user
	 :type string)
   (password :accessor password
	     :initarg :password
	     :type string))
  (:default-initargs :stylesheet "css/nexus.css"))

(defcomponent db-viewer (db-page)
  ())

(defmethod render ((page db-viewer))
  (<:h1 (<:ah (db page)))
  (with-database
    (dolist (table (list-tables))
      (<:p
	(<ucw:a :action (edit-table page table)
		(<:ah table))))
    ))


(defaction edit-table ((page db-page) table)
  (call 'db-table-viewer :table table
	:db (db page) :user (user page) :password (password page)))


(defcomponent db-table-viewer (db-page)
  ((table :accessor table
	  :initarg :table
	  :type string)
   (from :accessor from
	 :initform 0
	 :type integer)
   (results-per-page :accessor results-per-page
		     :initform 50
		     :type integer)
   ))

(defmethod render ((page db-table-viewer))
  (with-database
    (multiple-value-bind (rows titles) (select [*] :from (table page) :offset (from page) :limit (results-per-page page))
      (let* ((from (from page))
	     (step (results-per-page page))
	     (len (car (select [count [*]] :flatp t :from (table page)))))
	(declare (integer from step len))
	(<:table
	  :class "item-list" :cellspacing 0 :border 1
	  (<:tr
	    (item-list-header-margin)
	    (dolist (title titles)
	      (<:th (<:ah title))))
	  (let ((items rows))
	    (<ucw:form
	      (let ((deletions (mapcar (lambda (unused) (declare (ignore unused)) nil) items)))
		(do ((i 0 (1+ i))
		     (remainder (cdr items) (cdr remainder))
		     (item (car items) (car remainder)))
		  ((or (>= i step) (not item)))
		  (<:tr
		    (let ((i i))
		      (<:td (<ucw:input :type "checkbox" :accessor (nth i deletions)))
		      (dolist (datum item)
			(<:td (<:ah datum))))))
		(<:tr
		  (<:td :colspan 20 :align "left"
			(<ucw:submit :action (do-delete-records page titles items deletions)
				     :value "Delete")))))
	    (<:tr
	      :class "list-nav"
	      (<:td
		:colspan 20
		:align "center"
		(<:table
		  :class "list-positioner" :width "100%" :cellspacing 0
		  (<:tr
		    (<:td :width "10%" (<:ai "&nbsp;"))
		    (<:td
		      :align "center" :width "*"
		      (let* ((num-pages (ceiling len step))
			     (cur-page (floor from step)))
			(declare (integer num-pages cur-page))
			(when (> num-pages 1)
			  (<:table
			    :cellspacing "0"
			    :class "nav-wrapper"
			    (<:tr
			      (if (zerop cur-page)
				  (progn
				    (<:td (<:a :class "nav-begin" (<:img :src "images/begin-dis.gif")))
				    (<:td (<:a :class "nav-back" (<:img :src "images/back-dis.gif"))))
				  (progn
				    (<:td (<ucw:a :class "nav-begin"
						  :action (navigate-to page 0)
						  (<:img :src "images/begin.gif")))
				    (<:td (<ucw:a :class "nav-back"
						  :action (prev-items page)
						  (<:img :src "images/back.gif")))))
			      (let ((page-index cur-page))
				(<:td
				  :class "nav-page"
				  (<ucw:form
				    :action (navigate-to-page page (- page-index 1))
				    (<ucw:integer-range-select
				      :accessor page-index
				      :min 0 :max num-pages
				      :onchange "this.form.submit()")
				    (<:ah " of " num-pages)
				    (<ucw:submit :action (navigate-to-page page (- page-index 1))
						 :value "Go"))))
			      (if (equal cur-page (- num-pages 1))
				  (progn
				    (<:td (<:a :class "nav-next" (<:img :src "images/next-dis.gif")))
				    (<:td (<:a :class "nav-end" (<:img :src "images/end-dis.gif"))))
				  (progn
				    (<:td (<ucw:a :class "nav-next"
						  :action (next-items page)
						  (<:img :src "images/next.gif")))
				    (<:td (<ucw:a :class "nav-end"
						  :action (navigate-to page max-pages)
						  (<:img :src "images/end.gif"))))))))))
		    (<:td
		      :align "right" :width "10%"
		      (<ucw:form
			:action (do-nothing page)
			(<ucw:integer-range-select
			  :accessor (results-per-page page)
			  :min 0 :max 50
			  :onchange "this.form.submit()")
			#|(render
			  (make-instance 'drop-down-field
					 :value (results-per-page page)
					 :writer (lambda (x) (setf (results-per-page page) x))
					 :data-set '(("5" . 5)
						     ("10" . 10)
						     ("15" . 15)
						     ("30" . 30)
						     ("50" . 50))
					 :onchange "this.form.submit()"))|#
			)
		      )))))))))))


(defaction do-delete-records ((page db-page) fields items deletions)
  (do-delete-records% page (table page) fields items deletions))

(defun do-delete-records% (page table fields items deletions)
  (with-database
    (let ((i 0))
      (loop for delete in deletions
	    for item in items do
	    (when delete
	      (delete-records
		:from table
		:where (apply
			 #'sql-and
			 (loop for field in fields
			       for value in item collecting
			       [= (sql-expression :attribute field) value]))))
	    (incf i 1)))))



#.(clsql:restore-sql-reader-syntax-state)
