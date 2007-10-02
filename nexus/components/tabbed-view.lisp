#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

;;; The tabbed-view provides us with nice shiny tabs to make our interface prettier.

(in-package :nexus)


#.(clsql:locally-enable-sql-reader-syntax)


(defcomponent tabbed-view (paragent-component)
  ((contents :accessor contents
             :initform '()
             :type list
             :initarg :contents
             :documentation "A list of rendering functions for each tab.")
   (toolbar :accessor toolbar
            :type (or null function)
	    :initform nil
	    :initarg :toolbar
            :documentation "A function that renders above the the tabbed portion of the view.")
   (tabs :accessor tabs
         :initform '()
         :type list
         :initarg :tabs
         :documentation "The names of the tabs")
   (current-tab :accessor current-tab
		:initarg current-tab
		:type integer
		:initform 1)
   (tab-wrapper-class :accessor tab-wrapper-class
		      :initarg :tab-wrapper-class
                      :type string
		      :initform "box-title")
   (content-wrapper-class :accessor content-wrapper-class
			  :initarg :content-wrapper-class
                          :type string
			  :initform "box")
   (tabs-class :accessor tabs-class
               :initarg :tabs-class
               :type string
               :initform "tabs")
   (ids :accessor ids
        :type list
        :initform '()
        :documentation "Holds dom ids for each tab"))
  (:documentation "A tab control, obviously"))

(defmethod initialize-instance :after ((page tabbed-view) &key)
  (initialize-ids page))

(defmethod initialize-ids ((page tabbed-view))
  (setf (ids page)
        (mapcar
         (lambda (content)
           (declare (ignore content))
           (unique-id "tabbed-content"))
         (contents page))))

(defmethod render ((page tabbed-view))
  (<:div 
   :class (tab-wrapper-class page)
   (when (toolbar page)
     (funcall (toolbar page) page))
   (<:div :class (tabs-class page)
   (<:ul 
    (let ((first t))
      (loop for tab in (tabs page)
	   for id in (ids page)
	 counting tab into i do
	   (<:li :class (if first "first-item" "")
		 :id (format nil "~a-anchor" id)
		 :style (if (= i (current-tab page)) "display: none;" "")
		 (<:a :href "#"
		      :onclick (js:js-to-string
				`(progn
				   ,@(mapcar
				      (lambda (id)
					`(-element.hide ,id))
				      (ids page))
				   ,@(mapcar
				      (lambda (id)
					`(-element.hide ,(format nil "~a-selected" id)))
				      (ids page))
				   ,@(mapcar
				      (lambda (id)
					`(-element.show ,(format nil "~a-anchor" id)))
				      (ids page))
				   (-element.hide ,(format nil "~a-anchor" (nth (- i 1) (ids page))))
				   (-element.show ,(nth (- i 1) (ids page)))
				   (-element.show ,(format nil "~a-selected" (nth (- i 1) (ids page))))
				   (return false)))
		      (<:ah tab)))
	   (<:li :class (if first "first-item selected" "selected")
		 :id (format nil "~a-selected" id)
		 :style (if (= i (current-tab page)) "" "display: none;")
		 (<:span (<:ah tab)))
	   (setf first nil))))))
  (let ((first t))
    (loop for id in (ids page)
       for contents in (contents page)
       counting id into i do
	 (<:div :class (content-wrapper-class page)
	  :id id
	  :style (if (= i (current-tab page)) "" "display: none;")
	  (funcall contents page))
	 (setf first nil))))



(defcomponent vertical-tabbed-view (tabbed-view)
  ()
  (:documentation "A tabbed view that shows tab buttons vertically on the left,
 and tab contents on a pane to the right of them.")
  (:default-initargs
      :tabs-class "vertical-tabs"
      :tab-wrapper-class "vertical-tab-box-title"
    :content-wrapper-class "vertical-tab-box"))

(defmethod render :after ((page tabbed-view))
  (<:div :class "clear"))

#.(clsql:restore-sql-reader-syntax-state)
