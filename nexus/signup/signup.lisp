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

(defparameter *prices*
  `((,+account-free+ . 0)
    (,+account-basic+ . 0.81)
    (,+account-plus+ . 0.90)
    (,+account-premium+ . 0.99)))

(defun adjust-node-price (num-computers commitment)
  (let ((adjustment (if (>= commitment 12) 2/3 1)))
    (cond 
     ((>= num-computers 500) (* 2/3 adjustment))
     ((>= num-computers 250) (* 0.9 adjustment))
     (t adjustment))))
 
(defun price (account-type commitment num-computers)
  (* (cdr (assoc account-type *prices*)) (adjust-node-price num-computers commitment)))

(defun bulk-discount (num-computers account-type commitment)
  (float (* (- 1 (adjust-node-price num-computers commitment)) (price account-type commitment num-computers) num-computers)))

(defun price-for-computers (num-computers account-type commitment)
  (float (* (price account-type commitment num-computers) num-computers)))

(defun account-type-name (account-type)
  (case account-type
        (#.+account-free+ "Free")
        (#.+account-basic+ "Basic")
        (#.+account-plus+ "Plus")
        (#.+account-premium+ "Premium")))


(defentry-point "signup.ucw" (:application *my-app*)
    ()
  (call 'pricing-page :signup-fields (make-instance 'signup-fields)))

(defaction goto-signup ((page paragent-component))
  (call 'pricing-page :signup-fields (make-instance 'finished-signup-fields :user (user page))))


(defcomponent pricing-page (simple-window-component)
  ((num-computers :accessor num-computers
                  :initform (make-instance 'integer-field :value 100
                                           :dom-id "computer-count"
                                           :events '(("onchange" (update-prices))
                                                     ("onkeyup" (update-prices)))))
   (signup-fields :accessor signup-fields
                  :initarg :signup-fields)
   (commitment :accessor commitment
               :type integer
               :initform 12))
  (:default-initargs
    :title "Sign up"
    :stylesheet "css/signup.css"
    :message ""
    :javascript '((:src "prototype.js")
                  (:script "
function adjustNodePrice(numComputers) {
	var commitment = document.getElementById('commitment').selectedIndex;
	if(commitment > 0) {
		commitment = 0.6667;
	} else {
		commitment = 1.0;
	}
	if(numComputers >= 500) {
		return 0.66 * commitment;
	} else if(numComputers >= 250) {	
		return 0.90 * commitment;
	} else {
		return 1.0 * commitment;
  	};
};
function updatePrices() {
  var num = 0;
  if(document.getElementById('computer-count').value != '') { 
  	num = parseInt(document.getElementById('computer-count').value);
  }
  var discount = adjustNodePrice(num).toFixed(2);
  var premiumPrice = (discount * 0.99).toFixed(2);
  var plusPrice = (discount * 0.90).toFixed(2);
  var basicPrice = (discount * 0.81).toFixed(2);
  document.getElementById('premium-price').innerHTML = premiumPrice;
  document.getElementById('premium-total').innerHTML = (premiumPrice * num).toFixed(2);
  document.getElementById('plus-price').innerHTML = plusPrice;
  document.getElementById('plus-total').innerHTML = (plusPrice * num).toFixed(2);
  document.getElementById('basic-price').innerHTML = basicPrice;
  document.getElementById('basic-total').innerHTML = (basicPrice * num).toFixed(2);
};"))
    ))


(defmethod render ((page pricing-page))
  (<:body 
   :id "signup-page"
   (<:div 
    :id "wrapper"
    (<:div 
     :class "menu"
	(<:ul
	 (<:li (<:a :href "http://paragent.com/index.html"
		    (<:img :src "images/plogo_small.gif" 
			   :width "175px" 
			   :height "35px")))
	 (<:li (<:a :href "http://paragent.com/index.html" "Home"))
	 (<:li (<:a :href "http://paragent.com/blog/" "Blog"))
	 (<:li (<:a :href "http://paragent.com/community.html" "Community"))
	 (<:li (<:a :href "http://paragent.com/support.html" "Support"))))
    (<:div 
     :class "main"
     (<:h2 :class "pick" (<:span "Pick Your Plan"))
     (<:h3 "Paragent.com offers a number of plans to suit your needs. Whether it is a free plan to help you start getting organized, or the Premium service with integrated help desk and remote desktop, there is something for everyone. Pick your plan, and start using Paragent.com today.")
     (<ucw:form
      :id "signup"
      (macrolet
	  ((gen-table (&body cells)
	     (let ((even t))
	       `(progn
		  ,@(mapcar
		     (lambda (row)
		       (setf even (not even))
		       `(<:tr :class ,(if even "even" "odd")
			      ,@(mapcar
				 (lambda (item)
				   (let ((best nil)
					 (feature nil))
				     (when (and (listp item) (equal (car item) :best))
				       (setf best t)
				       (setf item (second item)))
				     (when (and (listp item) (equal (car item) :feature))
				       (setf feature t)
				       (setf item (second item)))
				     `(<:td :class ,(if best "best" 
							(if feature "feature" "")) :align "center" 
					    ,(cond 
					      ((equal item "X") `(<:img :src "images/okay.gif"))
					      ((equal item "-") `(<:ah " "))
					      (t `(<:ah ,item))))))
				 
				 row)))
		     cells)))))
	(<:table 
	 :id "pricing" :cellspacing 0 :cellpadding 0
	 (<:tr (<:th :class "feature"
		     (<:ah "Number of computers:"))
	       (<:th :class "computer-count" :colspan 4 (render (num-computers page))))
	 (<:tr (<:th )
	       (<:th :class "best" "Premium")
	       (<:th "Plus")
	       (<:th "Basic")
	       (<:th "Free"))
	 (gen-table
	  
	  ((:feature (<:as-is "Cost per Computer Per Month<span class=\"super\">*</span>")) 
	   (:best (<:span :id "premium-price" "$0.66"))
	   (<:span :id "plus-price" "$0.60") (<:span :id "basic-price" "$0.54") (<:ah "-"))
	  ((:feature "Computers") (:best "Unlimited") "Unlimited" "Unlimited" "5")
	  ((:feature "Hardware & Software Inventory") (:best "X") "X" "X" "X")
	  ((:feature "Audit") (:best "X") "X" "X" "X")
	  ((:feature "Reporting") (:best "X") "X" "X" "X")
	  ((:feature "Alerts") (:best "X") "X" "30 days" "30 days")
	  ((:feature "Help Desk") (:best "X") "X" "30 days" "30 days")
	  ((:feature "Remote Desktop") (:best "X") "30 days" "30 days" "30 days")
	  ("" (:best (progn (<:ah "$") (<:span :id "premium-total" 500)))
	      (progn (<:ah "$") (<:span :id "plus-total" 400))
	      (progn (<:ah "$") (<:span :id "basic-total" 300))
	      "Free")
	  ("" (:best (<ucw:input :action (continue-signup page +account-premium+) :class "signup"
										  :type "image" :src "images/signup.gif" :value "Sign up"))
	      (<ucw:input :action (continue-signup page +account-plus+) :class "signup"
									:type "image" :src "images/signup.gif" :value "Sign up")
	      (<ucw:input :action (continue-signup page +account-basic+) :class "signup"
									 :type "image" :src "images/signup.gif" :value "Sign up")
	      (<ucw:input :action (continue-signup page +account-free+) :class "signup"
									:type "image" :src "images/signup.gif" :value "Sign up"))
	  )
	 (<:tr (<:td )
	       (<:td :colspan 4
		     :class "footnote"
		     (<:ah "* with a ")
		     (<ucw:select
		      :id "commitment"
		      :accessor (commitment page)
		      :onchange "javascript:updatePrices();" :onkeyup "javascript:updatePrices();"
		      (<ucw:option :value 1 1)
		      (<ucw:option :value 12 12)
		      (<ucw:option :value 24 24))
		     (<:ah " month commitment.")))))
    
      (<ucw:script
       `(update-prices)
       )))
    (<:div :id "foot"
	   (<:p :class "legalese"
		"Copyright 2004-2007 Paragent, LLC. All rights reserved. | "
		(<:a :href "http://paragent.com/legal.html" "Legal") " | " 
		(<:a :href "http://paragent.com/support.html" "Contact Us"))))))

   

(defaction continue-signup ((page pricing-page) account-type)
  (when (and (validp (num-computers page)) (plusp (value (num-computers page))))
    (if (equal account-type 0)
        (call 'free-account-page)
        (call 'credit-card-page :num-computers (value (num-computers page)) :account-type account-type
              :signup-fields (signup-fields page)
              :commitment (commitment page))
        )))


(defcomponent credit-card-page (simple-window-component)
  ((num-computers :accessor num-computers
                  :initarg :num-computers
                  :type integer)
   (commitment :accessor commitment
               :initarg :commitment
               :type integer)
   (account-type :accessor account-type
                 :initarg :account-type
                 :type integer)
   (message :accessor message
            :initarg :message
            :type string
            :initform "")
   (signup-fields :accessor signup-fields
                  :initarg :signup-fields)
   (cc-fields :accessor cc-fields
              :initarg :cc-fields
              :initform (make-instance 'credit-card-fields)))
  (:default-initargs
    :title "Sign up"
    :stylesheet "css/signup.css"))

; Sign up for a trial

(defcomponent finished-signup-fields (paragent-component)
  ())

(defmethod render ((page finished-signup-fields))
  (let ((company (company (user page))))
    (<:fieldset
     :class "signup"
     (<:p "You are signing up for a paid account for " (<:ah (name company))))))


(defcomponent signup-fields ()
  ((message :accessor message
            :initform nil)
   (username :accessor username
             :initform (make-instance 'string-field
                                      :value ""
                                      :validators (list (make-instance 'not-empty-validator)
                                                        (make-instance 'e-mail-address-validator))))
   (password1 :accessor password1
             :initform (make-instance 'password-field
                                      :value ""
                                      :validators (list (make-instance 'not-empty-validator)
                                                        (make-instance 'length-validator :min-length 5))))
   (password2 :accessor password2
              :initform (make-instance 'password-field
                                       :value ""
                                       :validators (list (make-instance 'not-empty-validator)
                                                         (make-instance 'length-validator :min-length 5))))
   (company :accessor company
            :initform (make-instance 'string-field
                                     :value ""
                                     :validators (list (make-instance 'not-empty-validator))))))


(defmethod render ((page credit-card-page))
  (with-db
    (let ((num-computers (num-computers page))
          (account-type (account-type page))
          (commitment (commitment page))
          (cc-fields (cc-fields page))
          (signup-fields (signup-fields page)))
   (<:div 
    :id "wrapper"
    (<:div 
     :class "menu"
	(<:ul
	 (<:li (<:a :href "http://paragent.com/index.html"
		    (<:img :src "images/plogo_small.gif" 
			   :width "175px" 
			   :height "35px")))
	 (<:li (<:a :href "http://paragent.com/index.html" "Home"))
	 (<:li (<:a :href "http://paragent.com/blog/" "Blog"))
	 (<:li (<:a :href "http://paragent.com/community.html" "Community"))
	 (<:li (<:a :href "http://paragent.com/support.html" "Support"))))
    (<:div 
     :class "main"
     (<:h2 :class "create" (<:span "Create Your Account"))
     (<:h3 "Now that you have picked the plan you would like, it is time to create your account. First, we need to collect some account and billing information.")

      (<ucw:form
        :action (confirm-signup page cc-fields signup-fields)

        (<:fieldset
          :id "cc-pricing" :class "credit-card"
          (<:legend (<:ah "Plan: " (account-type-name account-type)))
          (<:table
            (<:tr (<:td "Commitment: ") (<:td (<:ah commitment) (<:ah (if (equal commitment 1) "month" " months"))))
            (<:tr (<:td "Computers: ") (<:td (<:strong (<:ah num-computers))))
            (<:tr (<:td "Price per computer per month:") (<:td (<:strong (<:ah "$" (format nil "~$" (price account-type commitment num-computers))))))
            (<:tr (<:td "") (<:td (<:hr)))
            (<:tr (<:td "Total monthly charge") (<:td (<:strong (<:ah "$" (format nil "~$" (price-for-computers  num-computers account-type commitment))))))))


        (render signup-fields)
        
        
        (<:div
          (<:fieldset
            :class "credit-card"
            (<:legend "Credit Card Information")
            (<:div :class "error" (<:ah (message page)))
            (let ()
              (setf (enabled cc-fields) t)
              (render cc-fields)
              (<ucw:input :action (confirm-signup page cc-fields signup-fields)
                          :class "button"
                          :type "image"
                          :src "images/nextbtn.gif")))))
      (<:div
        (<:fieldset
          :id "legalese" :class "credit-card"
          (<:legend "Terms")
          (<:ul :class "bullet"
                (<:li (<:p "All sales subject to the "
                           (<:a :href "http://paragent.com/hasa.html"
                                "Hosted Application Services Agreement")))
                (<:li (<:p "Monthly payments are charged at the beginning of each monthly period, beginning with the date of purchase"))
                (<:li (<:p "Annual plans bill monthly beginning with the date of purchase"))
                (<:li (<:p "Agent installer emailed immediately after purchase confirmation"))
                (<:li (<:p "Monthly plans may be cancelled at any time; cancellation either must be made 1) online, 2) in writing, by email with company response, or 3) by US mail sent to:")
                      (<:p :class "contact"
                           "Paragent, LLC" (<:br)
                           "2701 Enterprise Drive. Suite 109" (<:br)
                           "Anderson, IN 46013"(<:br)))
                (<:li (<:p "No Returns/Refunds"))))))
    (<:div :id "foot"
	   (<:p :class "legalese"
		"Copyright 2004-2007 Paragent, LLC. All rights reserved. | "
		(<:a :href "http://paragent.com/legal.html" "Legal") " | " 
		(<:a :href "http://paragent.com/support.html" "Contact Us")))))))



(defmethod validate-signup-account ((page finished-signup-fields))
  t)


(defmethod validate-signup-account ((page signup-fields))
  (with-db
    (let ((password1 (value (password1 page)))
          (password2 (value (password2 page)))
          (company (sanitize-company-name (value (company page))))
          (email (value (username page)))
          (success-p nil))
      (cond
        ((or (equal email "") (not (find #\@ email)) (not (find #\. email)))
         (setf (message page) "You must enter a valid email address"))
        ((not (equal password1 password2))
         (setf (message page) "The passwords you entered do not match"))
        ((< (length password1) 5)
         (setf (message page) "You must have at least 5 characters in your password"))
        ((equal company "")
         (setf (message page) "You must enter your company name"))
        ((select 'company :where [= [name] company] :limit 1)
         (setf (message page) "This company already has an account with Paragent.com"))
        ((select 'user :where [= [username] email] :limit 1)
         (setf (message page) "This email address is already registered with Paragent.com"))
        ((not (validp page))
         (setf (message page) "You must fill out all fields"))
        (t
          ;(let ((secret (db::add-company company email password1 email)))
            ;(declare (ignore secret))
            ;; todo: generate
          (setf (message page) "")
          (setf success-p t)))
      success-p
  )))

  

(defmethod validate-credit-card ((cc-fields credit-card-fields))
  (if (validp cc-fields)
      (progn
        (setf (message cc-fields) "")
        t
        ;(call 'credit-card-page2 :user (user page) :cc-fields cc-fields)
        )
      (progn
        (setf (message cc-fields) "You must fill out all fields")
        nil)))




(defcomponent credit-card-page2 (credit-card-page)
  ()
  (:default-initargs
    :title "Confirm"
    :stylesheet "css/signup.css"))


(defmethod render ((page credit-card-page2))
  (let ((num-computers (num-computers page))
        (account-type (account-type page))
        (commitment (commitment page))
        (cc-fields (cc-fields page))
        (signup-fields (signup-fields page)))
   (<:div 
    :id "wrapper"
    (<:div 
     :class "menu"
	(<:ul
	 (<:li (<:a :href "http://paragent.com/index.html"
		    (<:img :src "images/plogo_small.gif" 
			   :width "175px" 
			   :height "35px")))
	 (<:li (<:a :href "http://paragent.com/index.html" "Home"))
	 (<:li (<:a :href "http://paragent.com/blog/" "Blog"))
	 (<:li (<:a :href "http://paragent.com/community.html" "Community"))
	 (<:li (<:a :href "http://paragent.com/support.html" "Support"))))
    (<:div 
     :class "main"
     (<:h2 :class "ready" (<:span "We're Ready!"))
     (<:h3 "We have all the information we need to create your account. Click on the " (<:strong "Sign Up") " button to create your account"))
     (with-db
	 (<:div
	  (<:fieldset
	   :class "credit-card"
	   (<:legend "Credit Card")
	   (let ((cc-fields (cc-fields page)))
	     (setf (enabled cc-fields) nil)
	     (render cc-fields))
	   ))
       (<:fieldset
	:class "credit-card credit-card-confirm"
	(<:legend "Confirmation")
        (<:p 
	 (<:ah "You will be billed ")
	 (<:strong
	  (<:ah "$" (format nil "~$" (price-for-computers num-computers account-type commitment))))
	 (<:ah " on a monthly basis. Is this okay?"))
        (<ucw:form
	 (<ucw:input :action (finalize-signup page) 
		     :class "button"
		     :type "image" :src "images/signup.gif" :id "signup" :value "Sign up")))))
    (<:div :id "foot"
	   (<:p :class "legalese"
		"Copyright 2004-2007 Paragent, LLC. All rights reserved. | "
		(<:a :href "http://paragent.com/legal.html" "Legal") " | " 
		(<:a :href "http://paragent.com/support.html" "Contact Us")))))


(defaction confirm-signup ((page credit-card-page) cc-fields signup-fields)
  (when (and (validate-signup-account signup-fields)
             (validate-credit-card cc-fields))
    (call 'credit-card-page2 :cc-fields cc-fields :signup-fields signup-fields
          :num-computers (num-computers page) :commitment (commitment page)
          :account-type (account-type page))
    ))

(defaction finalize-signup ((page credit-card-page2))
  (if (finalize-signup% page)
      (redirect-after-signup (signup-fields page))
      (answer t)))

(defaction redirect-after-signup ((page signup-fields))
  (let* ((username (value (username page)))
         (password (value (password1 page))))
    (call-component nil (make-instance 'login-redirector :username username :password password))))

(defaction redirect-after-signup ((page finished-signup-fields))
  (call 'front-page :user (user page)))

(defmethod finalize-signup% ((page credit-card-page2))
  (let* ((num-computers (num-computers page))
         (account-type (account-type page))
         (commitment (commitment page))
         
         (cc-fields (cc-fields page))
         (signup-fields (signup-fields page))
         
         (cc-number (value (cc-number cc-fields)))
         (exp-month (value (exp-month cc-fields)))
         (exp-year (value (exp-year cc-fields)))
         (first-name (value (first-name cc-fields)))
         (last-name (value (last-name cc-fields)))
         (address (value (address cc-fields)))
         (city (value (city cc-fields)))
         (state (value (state cc-fields)))
         (zip-code (value (zip-code cc-fields)))
         (phone-number (value (phone-number cc-fields)))
         
         (price (price-for-computers num-computers account-type commitment)))
    (with-db
      (with-transaction
        ()
        (when (validate-signup-account signup-fields)
          (let ((subscription-id (do-credit-card-subscription :months commitment
                                                              :price price
                                                              :card-number cc-number
                                                              :exp-month exp-month :exp-year exp-year
                                                              :first-name first-name :last-name last-name
                                                              :address address :city city
                                                              :state state :zip zip-code)))
            (if subscription-id
                (let ((company (make-company signup-fields)))
                  (setf (level company) account-type)
                  (update-records-from-instance company)
                  (update-records-from-instance
                    (make-instance 'subscription :company-id (id company)
                                   :subscription-id subscription-id
                                   :name (format nil "~a ~a" first-name last-name)
                                   :num-computers num-computers :level account-type
                                   :commitment commitment :phone-number phone-number))
                  t)
                (progn
                  (setf (message cc-fields) "This credit card information was rejected")
                  nil))
  ))))))


(defmethod make-company ((page signup-fields))
  (let ((company-name (sanitize-company-name (value (company page))))
        (username (value (username page)))
        (password (value (password1 page))))
    (db::add-company company-name username password username)))

(defmethod make-company ((page finished-signup-fields))
  "No need to create the company; it already exists"
  (company (user page)))

(defentry-point "freesignup.ucw" (:application *my-app*)
    ()
  (call 'free-account-page))


(defcomponent free-account-page (simple-window-component)
  ((signup-fields :accessor signup-fields
                  :initarg :signup-fields
                  :initform (make-instance 'signup-fields)))
  (:default-initargs
    :title "Free account"
    :stylesheet "css/nexus.css"))

(defmethod render ((page free-account-page))
  (let ((signup-fields (signup-fields page)))
    (<:h2 :class "signup" "Getting Your Own Paragent.com Account is Easy")
    (<:ul
      :class "signup"
      (<:li (<:img :src "images/step1.gif")
            (<:p "Create your own account by filling out the form below. Once your account has been created, you will be automatically logged in."))
      (<:li (<:img :src "images/step2.gif")
            (<:p "We will generate a custom installer for the Paragent.com agent that will be sent to the email address you provide."))
      (<:li (<:img :src "images/step3.gif")
            (<:p "Install the Paragent.com agent on any computers you want to monitor. Begin using Paragent.com to see how it can help you today.")))
    
    (<:fieldset
     :class "signup"
     (<:legend "Account Information")
     (<ucw:form
      :action (do-signup page signup-fields ) :id "signup"
      (setf (value (password1 signup-fields)) "")
      (setf (value (password2 signup-fields)) "")
      (when (message signup-fields)
        (<:p :class "error"
             (<:as-html (message signup-fields))))
      
       (<:p (<:label "Email:")
            (render (username signup-fields)))
       (<:p (<:label "Password:")
            (render (password1 signup-fields)))
       (<:p (<:label "Confirm Password:")
            (render (password2 signup-fields)))
       (<:p (<:label "Company Name:")
            (render (company signup-fields)))
      (<:br)
      (<ucw:input :type "image" :src "images/signup.gif" :value "Sign up!" :id "free-account-signup" 
                  :class "image"
                  :action (do-signup page signup-fields))))))

(defaction do-signup ((page free-account-page) signup-fields)
  (when (do-signup% page signup-fields)
    (let ((username (value (username signup-fields)))
          (password (value (password1 signup-fields))))
      (call 'login-redirector :username username :password password))))

(defmethod do-signup% ((page free-account-page) signup-fields)
  (let ((username (value (username signup-fields)))
        (password1 (value (password1 signup-fields)))
        (company (sanitize-company-name (value (company signup-fields)))))
    (when (validate-signup-account signup-fields)
      (with-db
        (db::add-company company username password1 username)))))
  
                   

(defmethod render ((page signup-fields))
  (setf (value (password1 page)) "")
  (setf (value (password2 page)) "")
  (<:fieldset
   :class "signup"
   (<:legend "Account Information")
   (when (message page)
     (<:p :class "error"
          (<:b (<:as-html (message page)))))
   (<:p (<:label "Email:")
        (render (username page)))
   (<:p (<:label "Password:")
        (render (password1 page)))
   (<:p (<:label "Confirm Password:")
        (render (password2 page)))
   (<:p (<:label "Company Name:")
        (render (company page)))))




(defun sanitize-company-name (company-name)
  (remove-if (lambda (char) 
               (case char
                     ((#\" #\' #\\ #\& #\+ #\# #\* #\@ #\! #\` #\~ #\Newline #\.
                           #\? #\> #\< #\, #\: #\|)
                      t)
                     (otherwise nil)))
             company-name))

#.(clsql:restore-sql-reader-syntax-state)
                            


