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

(defun do-credit-card-subscription (&key (interval 1) 
				         (months 12) 
				         (price 20)
                                         (card-number "4007000000027")
                                         (exp-month "05") 
                                         (exp-year "2009")
                                         (first-name "John") 
                                         (last-name "Smith")
				         (company "Foo Bar")
				         (email "foo@bar.com")
				         (phone-number "1234567890")
                                         (address "") 
				         (city "")
                                         (state "") 
				         (zip ""))
  (let* ((url "https://api.authorize.net/xml/v1/request.api")
         (content-type "text/xml")
         (login *credit-card-login*)
         (password *credit-card-password*)
         (data (concatenate 'string "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<ARBCreateSubscriptionRequest 
xmlns=\"AnetApi/xml/v1/schema/AnetApiSchema.xsd\"> 
  <merchantAuthentication> 
    <name>" login "</name> 
    <transactionKey>" password "</transactionKey> 
  </merchantAuthentication> 
  <subscription>
    <name>Paragent.com subscription</name> 
    <paymentSchedule> 
      <interval> 
        <length>" (format nil "~a" interval) "</length> 
        <unit>months</unit> 
      </interval> 
      <startDate>" (multiple-value-bind (usec second minute hour day month year dow)
                     (decode-time (get-time))
                     (declare (ignore usec second minute hour dow))
                     (format nil "~d-~2,'0d-~2,'0d" year month day)) "</startDate>
      <totalOccurrences>" (format nil "~a" months) "</totalOccurrences> 
    </paymentSchedule> 
    <amount>" (format nil "~$" price) "</amount>
    <payment> 
      <creditCard> 
        <cardNumber>" card-number "</cardNumber> 
        <expirationDate>" exp-year "-" exp-month "</expirationDate> 
      </creditCard> 
    </payment> 
    <customer>
      <email>" email "</email>
      <phoneNumber>" phone-number "</phoneNumber>
    </customer>
    <billTo> 
      <company>" company "</company> 
      <firstName>" first-name "</firstName> 
      <lastName>" last-name "</lastName>
      <address>" address "</address>
      <city>" city "</city>
      <state>" state "</state>
      <zip>" zip "</zip>
    </billTo> 
  </subscription> 
</ARBCreateSubscriptionRequest>"))
	 (response (drakma:http-request url
					:content-type content-type
					:force-binary t
					:content data)))
    (let ((response-string (sb-ext:octets-to-string response :external-format :latin-1)))
      (record "~a" response-string)
      (if (search "<resultCode>Ok</resultCode>" response-string)
	  (values t (string-between response-string "<subscriptionId>" "</subscriptionId>"))
	  (values nil (string-between response-string "<resultCode>" "</resultCode>"))))))

(defun string-between (string start-seq end-seq)
  (declare (string string start-seq end-seq))
  (let ((start (search start-seq string))
        (end (search end-seq string)))
    (when (and start end)
      (subseq string (+ start (length start-seq)) end))))
    




(defcomponent credit-card-fields ()
  ((message :accessor message
            :initform nil)
   (enabled 
    :accessor enabled
    :initform t)
   (cc-type 
    :accessor cc-type
    :initform (make-instance 
	       'select-field
	       :data-set '("Visa" "Mastercard" "American Express")))
   (cc-number 
    :accessor cc-number
    :initform (make-instance 
	       'string-field
	       :validators (list (make-instance 'not-empty-validator)
	                         (make-instance 'length-validator :min-length 10 :max-length 50))))
   (exp-month 
    :accessor exp-month
    :initform (make-instance 
	       'select-field
	       :data-set '("01" "02" "03" "04" "05" "06" 
			   "07" "08" "09" "10" "11" "12")))
   (exp-year
    :accessor exp-year
    :initform (make-instance 
	       'select-field
	       :data-set '("2007" "2008" "2009" "2010" "2011" "2012" 
			   "2013" "2014" "2015" "2016" "2017" "2018"
			   "2019" "2020" "2021" "2022" "2023" "2024")))
   #|(cid 
    :accessor cid
    :initform (make-instance 
	       'string-field
	       :validators (list (make-instance 'not-empty-validator))))|#
   (first-name 
    :accessor first-name
    :initform (make-instance 
	       'string-field
	       :validators (list (make-instance 'not-empty-validator)
	                         (make-instance 'length-validator :max-length 50))))
   (last-name 
    :accessor last-name
    :initform (make-instance 
	       'string-field
	       :validators (list (make-instance 'not-empty-validator)
	                         (make-instance 'length-validator :max-length 50))))
   (address
    :accessor address
    :initform (make-instance 
	       'string-field
	       :validators (list (make-instance 'not-empty-validator)
	                         (make-instance 'length-validator :max-length 60))))
   #|(address2 
    :accessor address2
    :initform (make-instance 
	       'string-field))|#
   (city 
    :accessor city
    :initform (make-instance 
	       'string-field
	       :css-class "city" :value ""
	       :validators (list (make-instance 'not-empty-validator)
	                         (make-instance 'length-validator :max-length 40))))
   (state 
    :accessor state
    :initform (make-instance 
	       'select-field
	       :data-set '("AL" "AK" "AS" "AZ" "AR" "CA" "CO" "CT" "DE"
			   "DC" "FM" "FL" "GA" "GU" "HI" "ID" "IL" "IN"
			   "IA" "KS" "KY" "LA" "ME" "MH" "MD" "MA" "MI"
			   "MN" "MS" "MO" "MT" "NE" "NV" "NH" "NJ" "NM"
			   "NY" "NC" "ND" "MP" "OH" "OK" "OR" "PW" "PA"
			   "PR" "RI" "SC" "SD" "TN" "TX" "UT" "VT" "VI"
			   "VA" "WA" "WV" "WI" "WY")))
   (zip-code 
    :accessor zip-code
    :initform (make-instance 
	       'string-field
	       :validators (list (make-instance 'not-empty-validator)
	                         (make-instance 'length-validator :max-length 20))))
   (email 
    :accessor email
    :initform (make-instance 
	       'string-field
	       :validators (list (make-instance 'not-empty-validator))))
   (phone-number 
    :accessor phone-number
    :initform (make-instance 
	       'string-field
	       :validators (list (make-instance 'not-empty-validator)
	                         (make-instance 'phone-number-validator))))))


(defmethod render ((page credit-card-fields))
  (if (enabled page)
      (progn
        (when (message page)
                 (<:p :class "error"
                      (<:b (<:as-html (message page)))))
        
         (<:div
          :id "credit-card-info"
           (<:p (<:label "Credit Card:")
                (render (cc-type page)))
           (<:p (<:label "Credit Card Number:")
                (render (cc-number page)))
           (<:p :class "exp"
                (<:label "Expiration Date: (mm/yy)")
                (render (exp-month page))
                (render (exp-year page))))
         (<:div 
          :id "credit-card-address"
           (<:p (<:label "First Name:")
                (render (first-name page)))
           (<:p (<:label "Last Name:")
                (render (last-name page)))
           (<:p (<:label "Street Address:")
                (render (address page)))
           (<:p (<:label "City, State:")
                (render (city page))
                (render (state page)))
           (<:p :class "zip" 
                (<:label "Zip Code:")
                (render (zip-code page)))
           (<:p :class "email" 
                (<:label "Email:")
                (render (email page)))
           (<:p :class "phone" 
                (<:label "Phone Number:")
                (render (phone-number page)))))
      (progn
        (<:div :id "credit-card-info"
               (<:p (<:label "Credit Card:")
                    (<:ah (value (cc-type page))))
               (<:p (<:label "CC#:")
                    (let* ((cc-number (value (cc-number page)))
                           (len (length cc-number)))
                      (dotimes (i (- len 4))
                        (<:ah "X"))
                      (<:ah (subseq cc-number (- len 4) ))))
               (<:p (<:label "Expiration Date:")
                    (<:ah (value (exp-month page)))
		    (<:ah (value (exp-year page))))
               #|(<:p (<:label "CID#:")
                    (<:ah (value (cid page))))|#)
        
        (<:div :id "credit-card-address"
               (<:p (<:label "Name:")
                    (<:ah (value (first-name page)) " " (value (last-name page))))
               (<:p (<:label "Street Address:")
                    (<:ah (value (address page))))
               (<:p (<:label "City, State:")
                    (<:ah (value (city page)) ", " (value (state page))))
               (<:p (<:label "Zip Code:")
                    (<:ah (value (zip-code page))))
	       (<:p (<:label "Email:")
                    (<:ah (value (email page))))
	       (<:p (<:label "Phone Number:")
		    (<:ah (value (phone-number page))))))))

#.(clsql:restore-sql-reader-syntax-state)
