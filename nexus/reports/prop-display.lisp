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


(defgeneric render-prop-list (prop &key display-func)
  (:documentation "Displays the properties of this hardware object"))


(defun %render-prop-list (obj labels funcs)
  (declare (list labels funcs))
  (mapcar
    (lambda (label func)
      (<:p (<:b (<:as-html label ": "))
           (<:as-html (funcall func obj))))
    labels funcs))

(defun %render-prop-list-no-label (obj labels funcs)
  (declare (list labels funcs))
  (mapcar
    (lambda (label func)
      (declare (ignore label))
      (<:p (<:as-html (funcall func obj))))
    labels funcs))

(defmethod render-prop-list ((prop string) &key (display-func '%render-prop-list-no-label))
  (funcall display-func
	   prop
	   '(nil)
	   `(,(lambda (x) x))))

(defmethod render-prop-list ((prop ip-address) &key (display-func '%render-prop-list))
  (funcall display-func
    prop
    '("Address")
    '(name)))


(defmethod render-prop-list ((prop bios) &key (display-func '%render-prop-list))
  (funcall display-func
    prop
    '("Name" "Language" "Manufacturer" "Release-date" "Version" "Serial Number")
    '(name language manufacturer release-date version serial-number)))


(defmethod render-prop-list ((prop cd-rom) &key (display-func '%render-prop-list))
  (funcall display-func
    prop
    '("Name" "Drive")
    '(name drive)))

(defmethod render-prop-list ((prop logical-drive) &key (display-func '%render-prop-list))
  (funcall display-func
    prop
    '("Name" "Size" "Free Space")
    `(name ,(lambda (prop) (readable-byte-size (size prop))) ,(lambda (prop) (readable-byte-size (free-space prop))))))

(defmethod render-prop-list ((prop hard-drive) &key (display-func '%render-prop-list))
  (funcall display-func
    prop
    '("Name" "Size" "Interface Type")
    `(name ,(lambda (prop) (readable-byte-size (size prop))) interface-type)))

(defmethod render-prop-list ((prop hardware-error) &key (display-func '%render-prop-list))
  (funcall display-func
    prop
    '("Description")
    '(description)))

(defmethod render-prop-list ((prop hot-fix) &key (display-func '%render-prop-list))
  (funcall display-func
    prop
    '("Name" "Description" "Installed By")
    '(name description installed-by)))

(defmethod render-prop-list ((prop memory) &key (display-func '%render-prop-list))
  (funcall display-func
    prop
    '("Capacity" "Speed" "Form Factor")
    `(,(lambda (prop) (readable-byte-size (capacity prop))) memory-speed form-factor)))

(defmethod render-prop-list ((prop db::memory-array) &key (display-func '%render-prop-list))
  (funcall display-func
    prop
    '("Max Capacity" "Number of Slots" "Error Correction") 
    ; NOTE: this is a total hack - if the value is interpreted as
    ; KB, and it would result in a figure greater than 16GB, then 
    ; given todays hardware, it must be wrong, and it is really
    ; reporting the number of bytes. Otherwise, act as if it is
    ; in KB. This needs to be fixed on the agent side to get the
    ; Units value for an object and determine the correct adjustment
    `(,(lambda (prop)
	       (if (db::max-capacity prop)
		   (if (> (db::max-capacity prop) 16777216)
		       (readable-byte-size (db::max-capacity prop))
		       (readable-kbyte-size (db::max-capacity prop)))
		   "(no data)"))
      db::num-slots 
      ,(lambda (prop) (db::error-correction-string (db::error-correction prop))))))

(defmethod render-prop-list ((prop network-card) &key (display-func '%render-prop-list))
  (funcall display-func
    prop
    '("Name" "Mac-Address" "Manufacturer")
    '(name mac-address manufacturer)))

(defmethod render-prop-list ((prop operating-system) &key (display-func '%render-prop-list))
  (funcall display-func
    prop
    '("Name" "Version" "Product ID" "Registered User" "Service Pack")
    '(name version product-id registered-user service-pack)))

(defmethod render-prop-list ((prop printer) &key (display-func '%render-prop-list))
  (funcall display-func
    prop
    '("Name" "Default Printer?")
    '(name is-default)))

(defmethod render-prop-list ((prop processor) &key (display-func '%render-prop-list))
  (funcall display-func
    prop
    '("Name" "Info" "Architecture" "L2 Cache" "Clock Speed")
    '(name info architecture l2-cache clock-speed)))

(defmethod render-prop-list ((prop sound-device) &key (display-func '%render-prop-list))
  (funcall display-func
    prop
    '("Name" "Manufacturer")
    '(name manufacturer)))

(defmethod render-prop-list ((prop startup) &key (display-func '%render-prop-list))
  (funcall display-func
    prop
    '("Name" "Location" "Command" "User")
    '(name location command user)))

(defmethod render-prop-list ((prop motherboard) &key (display-func '%render-prop-list))
  (funcall display-func
    prop
    '("Name" "Manufacturer" "Serial Number")
    '(name manufacturer serial-number)))

(defmethod render-prop-list ((prop user-account) &key (display-func '%render-prop-list))
  (funcall display-func
    prop
    '("Name" "Locked" "Disabled")
    '(name locked disabled)))

(defmethod render-prop-list ((prop video-controller) &key (display-func '%render-prop-list))
  (funcall display-func
    prop
    '("Name" "Horizontal Resolution" "Vertical Resolution" "Refresh Rate" "Driver")
    '(name horizontal-resolution vertical-resolution refresh-rate driver)))

(defmethod render-prop-list ((prop software) &key (display-func '%render-prop-list))
  (funcall display-func
    prop
    '("Name" "Publisher" "Version")
    '(name publisher version)))


