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


(defvar *message* "")



;; Base classes with the slots we'll need pretty much everywhere

(defclass paragent-component ()
  ((user :accessor user
         :type (or user null)
         :initarg :user
         :documentation "What a person can see depends on their company, so we need to know who they are in most every component")))

(defcomponent paragent-window-component (ucw:simple-window-component paragent-component)
  ((message :accessor message
            :type (or string null)
            :initarg :message
            :initform ""))
  (:default-initargs
    :stylesheet (list "css/nexus.css")
    :javascript '((:script "djConfig.parseWidgets = false; djConfig.searchIds = [];") ;if you don't set this, things hang
;                  (:src "prototype.js")
;                  (:src "effects.js")
;                  (:src "paragent.js")
                  (:src "compressed.js")) ;compressed.js is a mashup of prototype.js, effects.js, and paragent.js
    ))

(defcomponent paragent-dialog-component (ucw:simple-window-component paragent-component)
  ((message :accessor message
            :type (or string null)
            :initarg :message
            :initform ""))
  (:default-initargs
    :stylesheet "css/dialog.css"
    :javascript '(;(:src "prototype.js")
                  ;(:src "paragent.js")
                  (:src "compressed.js")
                  (:src "selector.js")
                  
                  )))

(defcomponent paragent-container (container paragent-component)
  ())

(defgeneric body-id (window)
  (:documentation "Returns the id of the body of this page. Used to color the navigation buttons appropriately"))

(defmethod body-id ((component paragent-window-component))
  "Returns the id of the body of this page. Used to color the navigation buttons appropriately"
  "")


(defun render-status-box (page user company)
  "Displays the box in the top left corner that gives you at-a-glance information"
  (declare (ignore page))
  (declare (user user))
  (let* ((company-id (company-id user))
         (online (db-count 'computers :where [and [= [company-id] company-id]
                           [= [online] 1]]))
         (total (db-count 'computers :where [= [company-id] company-id])))
    (<:div :id "status"
           (<:ul (<:li :id "online" (link-to-online-computers
                                      (<:strong "Online: ") (<:as-html online)))
                 (<:li :id "offline" (link-to-offline-computers
                                       (<:strong "Offline: ") (<:as-html (- total online))))
                 (let ((software-audit-p (software-audit-p user)))
                   (<:li :id (if software-audit-p "softaudit_ok" "softaudit_failed")
                         (link-to-software (<:strong "Software Audit: ")
                                           (if software-audit-p
                                               (<:as-html "PASSED")
                                               (<:as-html "FAILED")))))
                 (when (>=account-plus company)
                   (<:li :id "open-tickets"
                         (<:a :href *url-my-open-tickets*
                              (<:strong "Open Tickets: ")
                              (<:ah (car (select [count [slot-value 'ticket 'id]] :from [tickets]
                                                 :flatp t
                                                 :where [and
                                                 [= [slot-value 'ticket 'company-id] (company-id user)]
                                                 [= [slot-value 'ticket 'assigned-user-id] (id user)]
                                                 [= [slot-value 'ticket 'state] 1]]))))))
                 ))))
    


;; Wrappers around our pages to handle the headers and footers

(defgeneric render-tasks (page)
  (:documentation "Renders the tasks section on the sidebar"))


(defmethod render-tasks ((page paragent-window-component))
  (<:h1 "Tasks")
  (<:ul
   (<:li (link-to-prefs "Manage User Accounts"))
   (<:li (link-to-alerts "Manage Alert Settings"))))

(defgeneric render-recent-computers (page)
  (:documentation "Renders the 'Recent Computers' section on the sidebar"))


(defmethod render-recent-computers ((page paragent-window-component))
  (let ((user (user page)))
    (when (recent-computers user)
      (<:h1 "Recent Computers")
      (<:ul
        (dolist (name (recent-computers user))
          (<:li (<:a :href (computer-link name) (<:ah name))))))))

(defgeneric render-header-footer (page body)
  (:documentation "Renders the header and footer for the page. Takes a function which renders the body."))

(defmethod render-header-footer ((page paragent-window-component) body)
  (with-db
    (let* ((user (user page))
           (company (company user)))
      (<:as-is "<!--[if lte IE 7]>")
      (<:link :rel "stylesheet" :type "text/css" :href "css/ie.css")
      (<:as-is "<![endif]-->")
      (<:as-is "<!--[if IE 7]>")
      (<:link :rel "stylesheet" :type "text/css" :href "css/ie7.css")
      (<:as-is "<![endif]-->")
      (<:script :type "text/javascript"
                (<:as-is
                  (js:js-to-string
                    `(setf document.body.id ,(body-id page)))))
      (<:div
        :id "header"
        (<:div :id "masthead"
               (<:a :href "http://www.paragent.com/"
                    (<:img :id "dfly" :src "images/plogo.gif" :alt "")))
        (<:div
          :id "info"
          (<:ul
            (<:li (<:as-html
                    (username user) " ("
                    (<ucw:a :action (do-logout page) "Log out") ")"))
            (<:li :id "account" (link-to-prefs "Setup"))
            (when (and (= (company-id user) 1) (> (level user) 4))
              (<:li :id "admin"
                    (<:a :href "/admin.ucw" "Administration")))
            (<:li :id "help"
                  (<:a :href "http://code.google.com/p/paragent/wiki/UserGuide" "Help"))
            (<:li :class "search"
                  (link-to-advanced-search "Search:"))
            (<:li :id "search"
                  (<:div
                    (<:form :method "GET" :action "search.ucw"
			    :onsubmit "javascript:flashMessage('Searching...')"
                            (<:text :class "ievahack" :name "q")))))))
      (<:div
        :id "content"
        (<:div
          :id "sidebar"
          (render-status-box page user company)
          (<:div :id "tasks" :class "sidesect"
                 (render-tasks page))
         (<:div :class "sidesect"
                (render-recent-computers page))
          (if (guest-p user)
              (progn
                (<:div
                 :id "signup"
                 :style "display:none;"
		 (<:p "Sign up for a 30-day trial account on our hosted service")
                 (<:a :href "freesignup.ucw" (<:img :src "images/signup.gif"
                                                    :width 86 :height 24
                                                    :alt "Sign up" :title "Sign up")))
                (<:script
                 (<:as-is
                  "Effect.Appear($('signup'), {duration:3});")))
              (when (and (>=admin user) (=account-free company))
                (<:div 
                 :id "signup"
                 (<:p "Upgrade to a paid account")
                 (<ucw:a :action (goto-signup page) (<:img :src "images/signup.gif" 
                                                           :alt "Sign up" :title "Sign up")))
                ))
          (<:div :id "sbfooter" (<:img :src "images/sidedfly.gif" :alt "")))
        (<:div :id "main"
               (<:div :id "navbar"
                      (<:li (link-to-main :id "dashnav"
                                          (<:span "Dashboard")))
                      (<:li (link-to-computers :id "compnav"
                                               (<:span "Computers")))
                      (<:li (link-to-software :id "softnav"
                                              (<:span "Software")))
                      (when (>=account-plus company)
                        (<:li (link-to-alerts :id "alertnav"
                                              (<:span "Alerts")))
                        (<:li (link-to-tickets :id "ticketsnav"
                                               (<:span "Tickets"))))
                      (<:li (link-to-reports :id "reportnav"
                                             :onmouseover "showMenu(this, 'reports-menu');"
                                             :onmouseout "hideMenu(this, 'reports-menu');"
                                             (<:span "Reports"))))
               (clsql:start-sql-recording)
               (<:div :id "workspace"
                      (funcall body))
               (clsql:stop-sql-recording)
               ))
      (<:div :id "footer"
             (<:p :class "legalese"
                  (<:as-is "Copyright &copy; 2004-2007 Paragent, LLC. All rights reserved.")))
      (<:table
        :id "reports-menu" :class "menu" :style "display:none; position:absolute;"
        :onmouseover "showMenu(null, 'reports-menu');"
        :onmouseout "hideMenu(null, 'reports-menu');"
        (<:tr (<:td (link-to-inventory "Hardware Inventory")))
        (<:tr (<:td (link-to-software-report "Software Inventory")))
        (<:tr (<:td (link-to-license-keys-report "License Keys")))
        (<:tr (<:td (link-to-warranty-report "Warranties")))
        (<:tr (<:td (link-to-event-report 7 "This Week's Events")))
        (<:tr (<:td (link-to-event-report 30 "This Month's Events")))
       (<:tr (<:td (<:hr)))
       (<:tr (<:td (link-to-tickets-overdue-report "Overdue Tickets")))
       (<:tr (<:td (link-to-tickets-due-today-report "Tickets due today")))
       (<:tr (<:td (link-to-tickets-new-report "New Tickets"))))
      (<:script :type "text/javascript"
                (<:as-is
                  (js:js
                    (let ((img1 (new -image))
                          (img2 (new -image))
                          (img3 (new -image))
                          (img4 (new -image))
                          (img5 (new -image))
                          (img6 (new -image)))
                      (setf img1.src "/images/compnavhov.gif")
                      (setf img2.src "/images/alertnavhov.gif")
                      (setf img3.src "/images/dashnavhov.gif")
                      (setf img4.src "/images/softnavhov.gif")
                      (setf img5.src "/images/reportnavhov.gif")
                      (setf img6.src "/images/ticketsnavhov.gif")))))
      (<:script :src "https://ssl.google-analytics.com/urchin.js" :type "text/javascript")
      (<:script :type "text/javascript"
                (<:as-is
                  (js:js (setf _uacct "UA-600197-2")
                         (urchin-tracker))))
      )))

(defaction do-logout ((page paragent-component))
  (call 'logout-redirector))

(defaction do-nothing ((page paragent-component))
  )

(defmacro defrender (((page class)) &rest body)
  "Wraps the navigation and sidebars around our page content"
  `(defmethod render ((,page ,class))
     (render-header-footer
       ,page
       (lambda ()
         ,@body))))





;; Handy little components


(defclass my-date-field (date-field)
  ()
  (:documentation "Date field which orders the inputs month/year"))

(defmethod render ((field my-date-field))
  (render (ucw::month field))
  (<:as-html "/")
  (render (ucw::year field)))


(defcomponent multiple-select-field (ucw:alist-select-field)
  ((size :accessor size
         :initarg :size
         :initform "1")
   (multiple :accessor multiple-p
             :initarg :multiple-p
             :initform t)))


(defmethod render ((field multiple-select-field))
  (<:select :name (make-new-callback
                    (lambda (value)
                      (setf (client-value field) value))
                    :name (ucw::name field))
            :title (ucw::tooltip field)
            :id (dom-id field)
            :tabindex (tabindex field)
            :class (css-class field)
            :style (css-style field)
            :size (size field)
            :multiple (if (multiple-p field)
                          "TRUE"
                          "FALSE")
            (setf (ucw::data-map field) (ucw::build-data-map field))
            (ucw::render-options field)))


(defun init-computer-selector (user)
  (declare (user user))
  "Must be called before using a computer-selector"
  (<:script :src "selector.js" :type "text/javascript")
  (<:script
    :type "text/javascript"
    (let ((company-id (company-id user))
          (computers (computers user)))
      (<:ai (format nil "var allComputers = new Object();~%"))
      (dolist (computer computers)
        (let ((computer (car computer)))
          (<:ai (format nil "allComputers[~a] = false;" (id computer)))))
      
      (<:ai (format nil "var tagsToComputers = new Hash();
tagsToComputers['All'] = [ ~{~a~#[~:;,~%~]~} ];~%"
                    (mapcar (lambda (computer)
                              (let ((computer (car computer)))
                                (format nil "{name: ~S, id: ~A}" (name computer) (id computer))))
                            computers)))
      (dolist (tag (get-tags company-id))
        (<:ai (format nil "tagsToComputers[~S] = [ ~{~a~#[~:;,~%~]~} ];~%"
                      tag
                      (mapcar
                        (lambda (computer)
                          (format nil "{ name: ~S, id: ~A }"
                                  (name computer)
                                  (id computer)))
                        (computers-for-tag tag company-id))))))))


(defcomponent computer-selector (paragent-container)
  ((filter :accessor filter)
   (selected :initarg :selected
             :accessor selected
             :type list
             :initform '())
   (computer-list-field :accessor computer-list-field
                        :type string
                        :initform ""))
  (:documentation "Important control which allows the user to select computers from their network"))


(defgeneric new-selection (selector)
  (:documentation "Returns the selected computer ids"))

(defmethod new-selection ((selector computer-selector))
  (let ((selected (split-sequence:split-sequence-if (lambda (c) (or (equal c #\space) (equal c #\+)))
                                                    (computer-list-field selector) :remove-empty-subseqs t)))
    (mapcar
      (lambda (num)
        (or (parse-integer num :junk-allowed t) -1))
      selected)))

(defgeneric new-selected-computers (selector user)
  (:documentation "Returns the the selected computers as computer objects"))

(defmethod new-selected-computers ((selector computer-selector) user)
  (declare (type user user))
  (let ((selected (new-selection selector)))
    (when selected
      (select 'computer :flatp t
              :where [and
              [= [company-id] (company-id user)]
              [in [id] selected]]))))


(defmethod initialize-instance :after ((page computer-selector) &key)
  (setf (filter page)
        (cons (cons "All" nil)
              (mapcar
                (lambda (tag)
                  (cons tag tag))
                (get-tags (company-id (user page))))))
  (setf (computer-list-field page)
        (format nil "~{~A~#[~:; ~]~}"
                (mapcar
                  (lambda (computer)
                    (id computer))
                  (selected page)))))

(defmethod render ((page computer-selector))
  (let ((filter-id (unique-id))
        (available-id (unique-id))
        (selected-id (unique-id))
        (computer-list-id (unique-id)))
    (<:div :class "computer-selector"
           (<:h3 "Computers:")
           (<:div :class "available"
                  (<:select :id filter-id :class "tag-filter"
                            (dolist (option (filter page))
                              (<:option (<:as-html (car option)))))
                  (<:select :id available-id :class "available" :size 10 :multiple t
                            :ondblclick (js:js-inline* `(add-selection ,available-id ,selected-id ,computer-list-id))))
           (<:ul :class "controls"
                 (<:li (<:a :href (js:js-inline* `(add-selection ,available-id ,selected-id ,computer-list-id))
                            :id "add" (<:img :src "images/add.gif")))
                 (<:li (<:a :href (js:js-inline* `(add-all-selection ,available-id ,selected-id ,computer-list-id))
                            :id "add-all" (<:img :src "images/add-all.gif")))
                 (<:li (<:a :href (js:js-inline* `(del-selection ,available-id ,selected-id ,computer-list-id))
                            :id "del" (<:img :src "images/remove.gif")))
                 (<:li (<:a :href (js:js-inline* `(del-all-selection ,available-id ,selected-id ,computer-list-id))
                            :id "del-all" (<:img :src "images/remove-all.gif"))))
           (<:div :class "selected"
                  (<:h3 "Selected")
                  (<:select :id selected-id :class "selected" :size 10 :multiple t
                            :ondblclick (js:js-inline* `(del-selection ,available-id ,selected-id ,computer-list-id))
                            (dolist (option (selected page))
                              (<:option :value (id option) (<:as-html (name option))))))
           (<ucw:input :type "hidden" :id computer-list-id :accessor (computer-list-field page))
           (<:script
             :type "text/javascript"
             (<:as-is
               (js:js*
                 `(setf (slot-value ($ ,filter-id) 'onchange) (make-filter ,filter-id ,available-id ,selected-id))))))))





;;; The collapsing box. We used to use it a lot

(defmacro render-collapsing-box (link-text header content &key (collapsed t) (header-class "") (content-class "") (scroll-speed 0.3) )
  "Makes us a nice collapsing box"
  `(render-collapsing-box%
     (lambda () ,link-text)
     (lambda () ,header)
     (lambda () ,content)
     :collapsed ,collapsed
     :header-class ,header-class
     :content-class ,content-class
     :scroll-speed ,scroll-speed))

(defun render-collapsing-box% (link-text header content &key (collapsed t) (header-class "") (content-class "") (scroll-speed 0.3))
  "Does the actual work to render the collapsing-box. Don't call this directly"
  (declare (boolean collapsed))
  (declare (string header-class content-class))
  (declare (number scroll-speed))
  (let* ((contents-id (unique-id "contents-"))
         (img-id (unique-id "img-")))
    (<:div :class header-class
          (<:a :href "#"
               :onclick (format nil "javascript: collapsingBox('~a', '~a', '~a'); return false;" contents-id img-id scroll-speed)
               (<:img :src (if collapsed "images/collapsed.gif" "images/expanded.gif")
                      :id img-id :height 9 :width 9 :alt "" :title "")
               (funcall link-text))
          (funcall header))
    (<:div
      :class content-class
      :id contents-id
      :style (if collapsed "display:none;" "")
      (funcall content))))

(defmacro render-collapsing-box2 (collapsed-p scroll-speed &body body)
  "Makes us a nice collapsing box. More flexible than the old render-collapsing-box."
  (let ((collapsed (gensym)))
    `(let ((contents-id (unique-id "contents-"))
           (img-id (unique-id "img-"))
           (scroll-speed ,(or scroll-speed 0.3))
           (,collapsed ,collapsed-p))
       (macrolet ((collapsing-link (&body body)
                    `(<:a :href "#"
                          :onclick (format nil "javascript: collapsingBox('~a', '~a', ~a); return false;" contents-id img-id scroll-speed)
                          ;(list ,collapsed ,,collapsed ,',collapsed)
                          (<:img :src (if ,',collapsed "images/collapsed.gif" "images/expanded.gif")
                                 :id img-id :height 9 :width 9 :alt "" :title "")
                          ,@body))
                  (collapsing-body (tag &body body)
                    (unless (listp tag) (setf tag (list tag)))
                    `(,@tag
                       :id contents-id
                       :style (if ,',collapsed "display:none;" "")
                       ,@body)))
         ,@body))))

(defmacro render-switching-box (&body body)
  "Helps render two panes that switch between each other"
  `(let ((front (unique-id "front-"))
         (back (unique-id "back-")))
     (macrolet ((render-front (&body body)
                  `(macrolet ((switch-link (&body body)
                                `(<:a :href "#"
                                      :onclick (format nil "javascript: Element.hide('~a'); Effect.Appear('~a'); return false;"
                                                       front back)
                                      ,@body)))
                     (<:div :id front
                            ,@body)))
                (render-back (&body body)
                  `(macrolet ((switch-link (&body body)
                                `(<:a :href "#"
                                      :onclick (format nil "javascript: Effect.Fade('~a', {afterFinish: function(obj) {Element.show('~a');}}); return false;"
                                                       back front)
                                      ,@body)))
                     (<:div :id back :style "display:none;"
                            ,@body))))
       ,@body)))


;;; Dialog functions

(defun init-thickbox ()
  (<:link :href "thickbox/thickbox.css" :rel "stylesheet" :type "text/css")
  (<:script :src "jquery.js" :type "text/javascript")
  (<:script :src "thickbox/thickbox.js" :type "text/javascript"))


(defun init-grey-box ()
  "This needs to be called before we do any dialog-related stuff"
  (<:link :href "greybox/greybox.css" :rel "stylesheet" :type "text/css")
  (<:script :src "greybox/AmiJS.js" :type "text/javascript")
  (<:script :src "greybox/greybox.js" :type "text/javascript")
  (<:script :type "text/javascript"
            (<:as-is
              (js:js
                (defvar *GB_IMG_DIR* "greybox/")
                (-grey-box.preload-grey-box-images)))))


(defmacro grey-box (title text action &optional (height 500) (width 500))
  "Displays the page called by the given action in a dialog box"
  `(<:a :href (format nil "~a" (make-link ,action))
        :onclick (format nil "return GB_show('~a', '~a', ~a, ~a)"
                         ,title
                         (make-link ,action)
                         ,height ,width)
        (<:as-html ,text)))

;;; A spin button

(defcomponent integer-spin-button ()
  ((client-value :accessor client-value
		 :initarg :value
		 :type integer)
   (writer :accessor writer
	   :initarg :writer
	   :type function)
   (max-value :accessor max-value
	      :initarg :max-value
	      :initform 50
	      :type integer)
   (min-value :accessor min-value
	      :initarg :min-value
	      :initform 1
	      :type integer)
   (onchange :accessor onchange
	     :initarg :onchange
	     :initform nil
	     :type string))
  (:documentation "Provides a nice spin-button to make it easy to select an integer.
We use it in the navigation bar of item-list."))

(defmethod value ((field integer-spin-button))
  (slot-value field 'client-value))

(defmethod (setf value) (new-val (field integer-spin-button))
  (let ((v (if (stringp new-val)
	       (let ((val (parse-integer new-val :junk-allowed t)))
		 (or val (min-value field)))
	       new-val)))
    (if (< v (min-value field))
	(setf v (min-value field))
	(when (> v (max-value field))
	  (setf v (max-value field))))
    (prog1
      (setf (slot-value field 'client-value) v)
      (when (slot-boundp field 'writer)
	(funcall (writer field) (value field))))))

(defmethod render ((field integer-spin-button))
  (let ((id (unique-id "spin-button")))
    (<:div :class "spin-button"
	   (<ucw:text :accessor (value field) :id id :onchange (onchange field))
	   (<:div :class "up"
		  (<:a :onclick
		       (js:js-to-string
			`(let ((val (parse-int (slot-value ($ ,id) 'value))))
			   (unless (>= val ,(max-value field))
			     (setf (slot-value ($ ,id) 'value) (+ val 1)))
			   (return false)))
		       :href "#"
		       (<:img :width "7px" :height "7px" :src "/images/up.gif" )))
	   (<:div :class "down"
		  (<:a :onclick
		       (js:js-to-string
			`(let ((val (parse-int (slot-value ($ ,id) 'value))))
			   (unless (<= val ,(min-value field))
			     (setf (slot-value ($ ,id) 'value) (- val 1)))
			   (return false)))
		       :href "#"
		       (<:img :width "7px" :height "7px" :src "/images/down.gif"))))))

;;; A better date field

(defclass calendar-date-field (date-field)
  ((id :accessor id
       :type string
       :initform (unique-id "calendardiv"))
   (calendar-id :accessor calendar-id
                :type string
                :initform (unique-id "calendar")))
  (:documentation "Date field with a dojo widget that allows easier date selection"))

(defmethod render ((field calendar-date-field))
  (<:span :class "calendar"
          (let* ((month (ucw::month field))
                 (month-id (dom-id month))
                 (day (ucw::day field))
                 (day-id (dom-id day))
                 (year (ucw::year field))
                 (year-id (dom-id year))
                 (calendar-id (calendar-id field))
                 (id (id field)))
            (render month)
            (<:as-html "/")
            (render day)
            (<:as-html "/")
            (render year)
            (let ((img-id (unique-id "calendarImage")))
              (<:a :href (format nil "javascript: toggleCalendar('~a', '~a', '~a', '~a', '~a', '~a');"
                                 month-id day-id year-id img-id id calendar-id)
                   (<:img :id img-id :src "/dojo/src/widget/templates/images/dateIcon.gif")))
            (<:div :id id :class "calendar" :style "display:none;"
                   (<:ai (format nil "<div widgetId='~a' id='~a' ><img style=\"margin: 20px 50px;\" src=\"/images/ajax-spinner.gif\" /></div>"
                                 calendar-id calendar-id)))
              )))



;;; Really just exists to get rid of the dialog and redirect us back to the main page.
(defaction goto-dialog-confirm ((page paragent-component) message redirect-to)
  (call-component nil (make-instance 'dialog-confirm :message message :redirect-to redirect-to)))

(defcomponent dialog-confirm (paragent-dialog-component)
  ((header :accessor header
	   :initarg :header
	   :type string
	   :initform "Confirm")
   (message :accessor message
            :initarg :message
            :type string
            :initform "Done")
   (redirect-to :accessor redirect-to
                :initarg :redirect-to
                :type string
                :initform "main.ucw"))
  (:documentation "Called by dialogs to dismiss the dialog and redirect appropriately."))

(defaction goto-dialog-alert ((page paragent-component) message redirect-to)
  (call-component nil (make-instance 'dialog-alert :message message :redirect-to redirect-to)))

(defcomponent dialog-alert (dialog-confirm)
  ()
  (:default-initargs
      :header "Error"))


;;; Shows a message to the user, and that's it.
(defaction goto-message-page ((page paragent-component) message)
  (call-component nil (make-instance 'message-page :message message)))

(defcomponent message-page (paragent-window-component)
  ((message :accessor message
            :initarg :message
            :type string)))

(defmethod render ((page message-page))
  (<:fieldset :id "message"
              (<:h3 (<:ah (message page)))))

(defmethod render ((page dialog-confirm))
  (<:script
    (<:as-is
      (js:js-to-string
        ;; Cause the base window to reload when we're done
        `(setf window.onload
               (lambda ()
                 (setf parent.window.location.href ,(redirect-to page))
                 (parent.-g-b_hide))))))
  (<:div :id "dialog"
	 (<:div :class "box-title" 
		(<:h2  (<:as-html (header page))))
	 (<:div :class "box"
		(<:p (<:as-html (message page)))
		(<:p (<:a :href (redirect-to page) 
			  (<:img :src "images/okaybtn.gif"))))))

(defmethod render ((page dialog-alert))
  (<:h2 :class "error" (<:as-html (header page)))
  (<:p :class "error" (<:as-html (message page)))
  (<:p (<:a :onclick (js:js-to-string
		      `(progn
			 (setf parent.window.location.href ,(redirect-to page))
			 (parent.-g-b_hide)))
	    :href (redirect-to page) 
	    (<:img :src "images/okaybtn.gif"))))



;; Csv component

(defmacro link-to-csv (page data text &key (link-class "csv") link-id)
  "Gives the link a .csv extension"
  `(let ((url (string-replace (make-link (goto-csv ,page ,data)) ".ucw?" ".csv?")))
     (<:img :src "images/csv.gif") (<:a :class ,link-class :id ,link-id :href url (<:as-html " " ,text))))

(defaction goto-csv ((page paragent-component) data)
  (call-component nil (make-instance 'csv-page :data data)))

(defcomponent csv-page ()
  ((data :accessor data
         :initarg :data
         :type list
         :initform nil))
  (:documentation "A page which ends up being a csv file. Takes care of the miming."))

(defmethod update-url ((page csv-page) url)
  (setf (ucw::uri.path url) "report.csv")
  url)

(defmethod render :before ((page csv-page))
  (setf (get-header (context.response *context*) "Content-Type") "text/csv")
  )

(defun escape-for-csv (item)
  "Make sure quotes are properly escaped in our csv"
  (if item
      (string-replace (format nil "~a" item) "\"" "\\\"")
      ""))

(defmethod render ((page csv-page))
  (dolist (row (data page))
    (<:ai "\"" (escape-for-csv (car row)) "\"" )
    (dolist (datum (cdr row))
      (<:as-is ",\"" (escape-for-csv datum) "\"" ))
    (<:as-is "
")))

#.(clsql:restore-sql-reader-syntax-state)
