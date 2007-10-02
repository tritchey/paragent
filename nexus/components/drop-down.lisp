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

(defmacro <drop-down-field (&key accessor data-set onchange test)
  `(render
     (make-instance 'drop-down-field
		    :value ,accessor
		    :writer (lambda (x) (setf ,accessor x))
		    :data-set ,data-set
		    :onchange ,onchange
		    :test ,test)))

(defcomponent drop-down-field ()
  ((client-value)
   (writer :accessor writer
	   :type function
	   :initarg :writer
	   :documentation "When the value is set, this function will be called with it")
   (data-set :accessor data-set
	     :type list
	     :initarg :data-set
	     :initform '())
   (onchange :accessor onchange
	     :type string
	     :initarg :onchange
	     :initform nil)
   (test :accessor test
	 :initarg :test
	 :initform #'equal))
  (:documentation "A prettier select field"))

(defmethod value ((field drop-down-field))
  (cdr (assoc (client-value field) (data-set field) :test (test field))))

(defmethod (setf value) (new-val (field drop-down-field))
  (setf (client-value field) (car (rassoc new-val (data-set field) :test (test field)))))

(defmethod client-value ((field drop-down-field))
  (slot-value field 'client-value))

(defmethod (setf client-value) (new-val (field drop-down-field))
  (setf (slot-value field 'client-value) new-val)
  (when (slot-boundp field 'writer)
    (funcall (writer field) (value field))))

(defmethod initialize-instance :after ((field drop-down-field) &key value)
  (unless (test field) (setf (test field) #'equal))
  (if value
      (setf (value field) value)
      (setf (slot-value field 'client-value) (caar (data-set field)))))


(defmethod render ((field drop-down-field))
  (let ((curr-id (unique-id "curr"))
	(menu-id (unique-id "menu"))
	(id (unique-id "drop-down")))
    (<ucw:input :type "hidden" 
		:accessor (client-value field) 
		:id id 
		:onchange (onchange field))
    (<:span 
     :class "drop-down"
     (<:a :class "curr" :id curr-id :href "#"
	  :onclick (format nil "showMenu(this.parentNode, '~a'); return false;" menu-id)
	  :onblur (format nil "setTimeout(\"hideMenu(null, '~a');\", 200);" menu-id)
	  (if (atom (client-value field))
	      (<:ah (client-value field))
	      (let ((key (car (client-value field)))
		    (value (cadr (client-value field))))
		(cond 
		  ((equal key :img)
		   (<:img :src value))))))
     (<:span 
      :id menu-id :class "menu-container" :style "display:none;"
      (<:ul :class "drop-down"
       :class "menu"
       :onmouseover (format nil "showMenu(null, '~a');" menu-id)
       (dolist (data (data-set field))
	 (let* ((key (car data))
		(value (cdr data))
		(js (js:js-to-string
		     `(let ((hidden ($ ,id))
			    (curr ($ ,curr-id)))
		       (setf (slot-value hidden 'value) ,key)
		       (setf (slot-value curr 'inner-h-t-m-l) ,key)
		       (setf (slot-value curr 'value) ,key)
		       (hide-menu null ,menu-id)
		       (.onchange hidden)
		       (return false)))))
	   (if value
	       (<:li :onclick js
		     (<:a :href "#"
			  :onclick js
			  (<:ah key)))
	       (<:li :class "menu-header" (<:ah key))))))))))
	  
(defcomponent drop-down-number-range-field (drop-down-field)
  ((min-value :accessor min-value
	      :initarg :min-value
	      :type integer)
   (max-value :accessor max-value
	      :initarg :max-value
	      :type integer))
  (:documentation "A prettier integer select field"))


(defmethod initialize-instance :after ((field drop-down-number-range-field) &key value)
  (if value
      (setf (value field) value)
      (setf (slot-value field 'client-value) (min-value field))))

(defmethod value ((field drop-down-number-range-field))
  (let ((client-value (client-value field)))
    (if (stringp client-value)
	(let ((val (parse-integer client-value :junk-allowed t)))
	  (or val (min-value field)))
	client-value)))

(defmethod (setf value) (new-val (field drop-down-number-range-field))
  (setf (client-value field) new-val))

(defmethod client-value ((field drop-down-number-range-field))
  (slot-value field 'client-value))

(defmethod (setf client-value) (new-val (field drop-down-number-range-field))
  (setf (slot-value field 'client-value) new-val)
  (when (slot-boundp field 'writer)
    (funcall (writer field) (value field))))



(defmethod render ((field drop-down-number-range-field))
   (let ((curr-id (unique-id "curr"))
	 (menu-id (unique-id "menu"))
	 (id (unique-id "drop-down"))
	 (max (max-value field))
	 (min (min-value field)))
    (<ucw:input :type "hidden" :accessor (client-value field) :id id :onchange (onchange field))
    (<:span :class "drop-down"
	    (<:a :class "curr" :id curr-id :href "#"
		 :onclick (format nil "showMenu(this.parentNode, '~a'); return false;" menu-id)
		 :onblur (format nil "setTimeout(\"hideMenu(null, '~a');\", 200);" menu-id)
		 (<:ah (client-value field) ) (<:ai "&nabla;"))
	    (<:span
	      :id menu-id :class "menu-container" :style "display:none;"
	      (<:table
		:class "menu"
		:onmouseover (format nil "showMenu(null, '~a');" menu-id)
		;:onmouseout (format nil "hideMenu(null, '~a');" menu-id)
		(loop for i from min to max do
		      (let ((js (format nil "var hidden = $('~a');
hidden.value = ~a;
$('~a').innerHTML = ~a;
hideMenu(null, '~a');
hidden.onchange();
return false;" id i curr-id i menu-id)))
			(<:tr (<:td :onclick js
				    (<:a :href "#"
					 :onclick js
					 (<:ah i))
				    )))))))))
	  


#.(clsql:restore-sql-reader-syntax-state)
