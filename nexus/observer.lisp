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


(defcomponent observer-page (paragent-window-component)
  ((computer :accessor computer
             :initarg :computer
             :type string)
   (password :accessor password
             :initarg :password
             :type string)
   (ticket :accessor ticket
	   :initarg :ticket
	   :type string)
   (server :accessor server
           :initarg :server
           :type string
           :initform *default-dark-archon-server*))
  (:documentation "Displays our vnc page, allowing you to remote into a computer."))


(defmethod render ((page observer-page))
  (let ((user (user page)))
    (with-db
	(<:div :id "applet" :style "padding: 0; margin: 0;"
	       (<:applet :code "com/paragent/observer/VncViewer.class"
			 :id "observer-applet"
			 :archive "observer.jar"
			 :width "100%" :height "100%"
			 (<:param :name "computer" :value (computer page))
			 (<:param :name "company" :value (name (company user)))
			 (<:param :name "password" :value (password page))
			 (<:param :name "ticket" :value (ticket page))
			 (<:param :name "host" :value (server page)))))
    (<:script
     :type "text/javascript"
     (<:as-is
      (js:js
       (progn
	 (defun resize ()
	   (let ((size (get-window-size))
		 (new-height (- (slot-value size 'height) 30))
		 (new-width (- (slot-value size 'width) 30))
		 (applet ($ "observer-applet"))
		 (div ($ "applet")))
	     (setf (slot-value div 'style.width) (+ new-width "px"))
	     (setf (slot-value div 'style.height) (+ new-height "px"))
	     (.scroll window 0 0)))
	 (resize)))))))
         

