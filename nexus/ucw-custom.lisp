#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

;;;; This file exists to hold any customizations we need to make to ucw or clsql


(in-package :it.bese.ucw)


(defun rfc2109:cookie-string (name value &key comment domain max-age path secure (corrects-path-p nil))
  (and (rfc2109::correct name rfc2109::valid-name? "must be a valid name")
       (rfc2109::try-quotes value rfc2109::value? (rfc2109::correct value rfc2109::value? "must be a value"))
       (rfc2109::optional comment (rfc2109::try-quotes comment rfc2109::value?
			   (rfc2109::correct comment rfc2109::value? "must be a value")))
       (rfc2109::optional domain (rfc2109::try-quotes domain rfc2109::valid-domain?
			  (rfc2109::correct domain rfc2109::valid-domain? "must be an explicit valid domain")))
       (rfc2109::optional max-age (rfc2109::correct max-age (and (integerp max-age) (> max-age 0)) "must be an integer greater than 0"))
       (if corrects-path-p
	   (rfc2109::optional path (rfc2109::try-quotes path rfc2109::value?
				      (rfc2109::correct path rfc2109::value? "must be a value")))
	   (rfc2109::optional path path))
       (rfc2109::correct secure (or (eql secure t) (eql secure nil)) "must be t or nil"))
  (let ((cookie-string
	 (format nil "~A=~A~@[;comment=~A~]~@[;domain=~A~]~@[;max-age=~A~]~@[;expires=~A~]~@[;path=~A~]~@[;secure~];version=1"
	         name value comment domain max-age
	         (when max-age
	           (cookie-date (clsql-sys::time+ (clsql:get-time)
	                                          ; add 4 hours for GMT
	                                          (clsql:make-duration :hour 4 :second max-age))))
	          path secure)))
    (when (rfc2109::cookie-string-too-long? cookie-string)
      (warn 'cookie-string-exceeds-minimum-length :cookie-string cookie-string))
    cookie-string))

(defun cookie-date (time)
  (multiple-value-bind (usec second minute hour day month year dow)
      (clsql-sys:decode-time time)
    (declare (ignore usec))
    (format nil "~a, ~2,'0d-~a-~2,'0d ~2,'0d:~2,'0d:~2,'0d GMT"
            (clsql-sys::day-name dow)
            day (subseq (clsql-sys::month-name month) 0 3) year
            hour minute second)))
            

(defmethod ucw::validate ((field date-field) (validator not-empty-validator))
  (and (validp (ucw::day field))
       (validp (ucw::month field))
       (validp (ucw::year field))
       (value field)))

(defmethod ucw::javascript-check ((field date-field) (validator not-empty-validator))
  `(not (= "" value)))


;; Keeps us from displaying precious debug information to the user
(defmethod ucw::window-component.title ((err ucw::error-component))
  "An error has occurred")

(defmethod render ((err ucw::error-component))
  (<:h3 "An error has occurred")
  (<:p "A technician has been notified of the error. We apologize for any inconvenience this has caused. Please try again later.")
  
  (handler-case
    (let* ((url (ucw::raw-uri (ucw::context.request *context*)))
           (error (ucw::error.condition err))
           (frames (loop 
                     for frame in (ucw::error.backtrace err)
                     for index upfrom 0
                     for div-id = (concatenate 'string "frame-" (princ-to-string index) "-details")
                     collect (ucw::backtrace-frame-description frame)))
           (body (format nil "Error: ~a~%Url: ~a~%Bactrace:~{~a~}"
                         error
                         url
                         frames)))
      (db:send-email db:*administrator-notification-email*
		  "Error in Nexus"
		  body))
    (simple-error ()
                  (format t "Error occurred trying to send email"))))



;; Keeps others from accessing the ucw admin page
(defmethod ucw::check-credentials ((login ucw::admin-login))
  nil)


;; Date field handlers

(defmethod (setf value) ((value clsql-sys::wall-time) (field date-field))
  (multiple-value-bind (year month day) (clsql-sys::time-ymd value)
    (setf (value (ucw::month field)) month)
    (setf (value (ucw::day field)) day)
    (setf (value (ucw::year field)) year)))

(defmethod (setf value) ((value clsql-sys::date) (field date-field))
  (multiple-value-bind (day month year) (clsql-sys::decode-date value)
    (setf (value (ucw::month field)) month)
    (setf (value (ucw::day field)) day)
    (setf (value (ucw::year field)) year)))

(defgeneric value-as-time (field)
  (:documentation "Returns the field's value as a time"))

(defmethod value-as-time ((field date-field))
  (if (and (value (ucw::year field)) (< (value (ucw::year field)) 100))
      (incf (value (ucw::year field)) 2000))
  (let ((utime (value field)))
    (when utime
      (clsql-sys:utime->time utime))))

(defgeneric value-as-date (field)
  (:documentation "Returns the field's value as a date"))

(defmethod value-as-date ((field date-field))
  (let ((time (value-as-time field)))
    (when time
      (clsql-sys::time->date time))))

(defmacro inner-call (component-type &rest component-init-args)
  "Kind of like call, but for use where the component is inserted into the page in an ajaxy way.
   Makes answer work like normal in such situations"
  (rebinding (component-type)
    `(etypecase ,component-type
       ((or symbol standard-component-class)
        (call-component (context.window-component *context*) (make-instance ,component-type ,@component-init-args)))
       (component
        (call-component (context.window-component *context*) ,component-type)))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (yaclml::def-html-tag <:applet :core code archive width height)
  
  (yaclml::def-html-tag <:param name value)
 
  (yaclml::def-html-tag <:th :core :event :i18n
              abbr
              align
              axis
              char
              charoff
              colspan
              headers
              rowspan
              scope
              valign
    width)
  
  (yaclml::def-html-tag center :core :i18n))


(export '(inner-call value-as-date value-as-time))
