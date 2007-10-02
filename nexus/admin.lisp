;;;; Created on 2006-09-29 11:14:56


(in-package :nexus)

#.(clsql:locally-enable-sql-reader-syntax)

(defentry-point "admin.ucw" (:application *my-app*)
  ()
  (let ((user (get-user)))
    (if (and user (= (company-id user) 1) (> (level user) 4))
        (call 'admin-page :user (get-user))
        (call 'redirect-component :target "login.ucw"))))


(defcomponent message-component (paragent-component widget-component)
  ())

(defmethod render ((page message-component))
  (<:br) (<:hr) (<:br)
  (<ucw:form 
    :action (do-nothing page)
    (<:p (<:ah "System-wide message: "))
    (<:p (<ucw:textarea :rows 25 :cols 80 :accessor *message*))
    (<ucw:submit :action (do-nothing page) :value "Submit")))

(defcomponent scarab-message-component (paragent-component widget-component)
  ())

(defmethod render ((page scarab-message-component))
  (<:br) (<:hr) (<:br)
  (<ucw:form 
    :action (do-nothing page)
    (<:p (<:ah "Scarab Messages: "))
    (<:p (<ucw:textarea :rows 25 :cols 80 :accessor *message*))
    (<ucw:submit :action (do-nothing page) :value "Submit")))

(defcomponent stats-component (paragent-component widget-component)
  ())

(defmethod render ((page stats-component))
  (flet ((render-companies (title companies)
           (<:br)
           (<:h3 (<:ah title))
           (<:table
             :class "sortable" :id "company-table" :border 1
             (<:tr (<:th "ID") (<:th "Company") (<:th "Computers") (<:th "Users")
                   (<:th "Created") (<:th "Last Login") (<:th "Account Type") (<:th ""))
             (dolist (company companies)
               (<:tr
                 (<:td (<:ah (id company)))
                 (<:td
                   (<ucw:a :action (show-company-details page company)
                           (<:ah (name company))))
                 (<:td (<:ah (db-count 'computers :where [= [company-id] (id company)])))
                 (<:td (<:ah (db-count 'users :where [= [company-id] (id company)])))
                (<:td (<:ah (created company)))
                 (<:td
                   (let ((last-login (car (select [max [last-login]] :from [users] :flatp t
                                                  :where [= [company-id] (id company)]))))
                     (<:ah last-login)))
                 (let ((company company))
                   (<:td (<ucw:form :action (save-company page company)
                                    (<ucw:select 
                                      :accessor (level company)
                                      :onchange "this.form.submit();"
                                      (<ucw:option :value +account-free+ "Free")
                                      (<ucw:option :value +account-basic+ "Basic")
                                      (<ucw:option :value +account-plus+ "Plus")
                                      (<ucw:option :value +account-premium+ "Premium"))))
                   (<:td (if (disabled company)
                             (<ucw:a :action (enable-account page company) "Enable")
                             (<ucw:a :action (disable-account page company) "Disable")))
                   ))))))
    (with-db
      (<:script :src "sorttable.js" :type "text/javascript")
      (dolist (info `(("Free" ,+account-free+)
                      ("Basic" ,+account-basic+)
                      ("Plus" ,+account-plus+)
                      ("Premium" ,+account-premium+)))
        (render-companies (first info) (select 'company :flatp t :order-by [name]
                                               :where [and
                                               [= [disabled] 0]
                                               [= [level] (second info)]])))
      (<:br)
      (<:hr)
      (render-companies "Disabled" (select 'company :flatp t :order-by [name]
                                           :where [= [disabled] 1]))
      )))

(defaction save-company ((page stats-component) company)
  (save-company% company))

(defun save-company% (company)
  (with-db
    (update-records-from-instance company)))

(defaction show-company-details ((page stats-component) company)
  (call 'company-stats-component :company company))

(defcomponent company-stats-component (paragent-component widget-component)
  ((company :accessor company
            :initarg :company)
   (message :accessor message
            :initform "")))

(defmethod render ((page company-stats-component))
  (with-db
    (<:p (<:b (<:ah (message page))))
    (let ((company (company page)))
      (<:br)
      (<:p (<:h3 (<:ah (name company)) " " ;(<:a :href "admin.ucw" "(All Companies)")))
                 (<ucw:a :action (do-answer page) "(All companies)")))
      (<:p (<:ah "Computers: " (length (computers company))))
      (<:br)
      (flet ((render-user-table (users)
               (<:table
                 :border 1
                 (<:tr (<:th "Username") (<:th "Last Login") (<:th " "))
                 (dolist (user users)
                   (let ((month-ago (time- (get-time) (make-duration :month 1))))
                     (<:tr (<:td (<:ah (username user)))
                           (<:td (<:span :class (if (time< (last-login user) month-ago)
                                                    "error"
                                                    "")
                                         (<:ah (last-login user))))
                           (let ((user user))
                             (<:td (<ucw:a :action (reset-user-password page user) "Reset password")))
                           ))))))
        (<:p (<:h3 "Users"))
        (render-user-table (users company))

      ))))

(defaction disable-account ((page stats-component) (company company))
  (disable-account% company))

(defgeneric disable-account% (company))

(defmethod disable-account% ((company company))
  (with-db
    (setf (disabled company) t)
    (update-records-from-instance company)))

(defaction enable-account ((page stats-component) (company company))
  (enable-account% company))

(defgeneric enable-account% (company))

(defmethod enable-account% ((company company))
  (with-db
    (setf (disabled company) nil)
    (update-records-from-instance company)))

(defaction reset-user-password ((page company-stats-component) user)
  (setf (message page) (if (admin-reset-user-password user)
                           "The user has been sent an email with their new password"
                           "The password has been changed, but we were unable to send an email. Please contact a developer")))


(defun admin-reset-user-password (user)
  (let ((password (random-password)))
    (with-db
      (setf (password user) (hash-password user password))
      (update-records-from-instance user))
    (send-email (list (email user)) "Paragent Password" 
                (format nil
                        "Your Paragent password has been changed to '~a'. After logging in, you can change it at https://archon.paragent.com/preferences.ucw"
                        password))))

(defaction do-answer ((page company-stats-component))
  (answer t))

(defcomponent add-company-component (paragent-component widget-component)
  ((message :accessor message
            :initform "")))

(defmethod render ((page add-company-component))
  (let ((company-name "")
        (admin-username "")
        (admin-password "")
        (admin-email ""))
    (<:p (<:as-html (message page)))
    (<:br) (<:hr) (<:br)
    (<:h3 "Add Company")
    (<ucw:form
      :action (do-add-company page company-name admin-username admin-password admin-email)
      (tableify
        ("Company: " (<ucw:text :accessor company-name))
        ("Admin username: " (<ucw:text :accessor admin-username))
        ("Admin email: " (<ucw:text :accessor admin-email))
        ("Admin password: " (<ucw:text :accessor admin-password)))
      (<ucw:submit
        :action (do-add-company 
		    page company-name admin-username admin-password admin-email)
        :value "Add company"))))


(defcomponent admin-page (paragent-window-component)
  ((switcher 
    :accessor switcher
    :component (tabbed-pane 
		:current-component-key "Statistics"
		:contents `(("Statistics" . ,(make-instance 'stats-component))
			    ("Add company" . ,(make-instance 'add-company-component))
			    ("Global Message" . ,(make-instance 'message-component)))))))

(defmethod render ((page admin-page))
  (<:a :href "main.ucw" "Back to Archon")
  (<:h2 "Admin")
  (<:br)(<:hr)(<:br)
  (render (switcher page)))


(defaction do-add-company ((page paragent-component) company-name admin-username admin-password admin-email)
  (%do-add-company page company-name admin-username admin-password admin-email))

(defun %do-add-company (page company-name admin-username admin-password admin-email)
  (with-db
      (setf (message page)
	    (format nil "Company added. Secret: ~a"
		    (db::add-company company-name 
				     admin-username 
				     admin-password 
				     admin-email)))))



#.(clsql:restore-sql-reader-syntax-state)