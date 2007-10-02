#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#


;;; The item-list merits some discussion, as we use it all over the place to
;;;  display most of our data.
;;; item-list gets its data from either optimized-list-getter or unoptimized-list-getter,
;;;  depending on whether or not use-optimized is true. If you use optimized-list-getter,
;;;  you must also implement optimized-list-count, so that item-list can know how many pages
;;;  it has. We use the optimized version in almost all cases, because selecting thousands of 
;;;  objects from the database is wildly inefficient when you only want ten.
;;; For each item to be displayed, render-item is called. Please note that you can look at 
;;;  the odd-row slot whenever this is called to style your row accordingly.
;;; If there are no items, render-none is called. By default, this displays no-message.
;;;
;;; See also the filtered-item-list


(in-package :nexus)

#.(clsql:locally-enable-sql-reader-syntax)



(defun item-list-margin ()
  (<:td :class "item-list-margin" :width "20px" (<:ai "&nbsp;")))

(defun item-list-header-margin ()
  (<:th :class "item-list-margin" :width "20px" (<:ai "&nbsp;")))

(defcomponent item-list (paragent-container)
  ((div-class :accessor div-class
              :initarg :div-class
              :type string
              :initform "item-list")
   (div-id :accessor div-id
           :initarg :div-id
           :type string
           :initform "")
   (cellspacing :accessor cellspacing
                :initarg :cellspacing
                :type integer
                :initform 0)
   (condensed :accessor condensed
	       :initarg :condensed
	       :initform nil)
   (show-title :accessor show-title
                :type boolean
                :initarg :show-title
                :initform t)
   (none-message :accessor none-message
                 :initarg :none-message
		 :type string
                 :initform "")
   (use-optimized :accessor use-optimized
                  :initarg :use-optimized
                  :type boolean
                  :initform nil)
   (from :accessor from
         :initarg :from
         :type integer
         :initform 0)
   (results-per-page :accessor results-per-page
                     :initarg :results-per-page
                     :type integer
                     :initform 10)
   (results-per-page-options :accessor results-per-page-options
                             :initarg :results-per-page-options
                             :type list
                             :initform (list 5 10 15 30 50))
   (odd-row :accessor odd-row
            :initform t
            :type boolean
            :documentation "used to keep track of what row we're on for styling purposes"))
  (:documentation "Displays a list of items, along with a navigation bar."))

(defmethod initialize-instance :after ((page item-list) &key)
  (let ((step (results-per-page page))
	(div-class (div-class page)))
    (when (condensed page)
      (setf (div-class page) (format nil "~a condensed" div-class)))
    (unless (find step (results-per-page-options page) :test #'equal)
      (setf (results-per-page-options page)
            (sort (cons step (results-per-page-options page)) #'<)))
    (setf (results-per-page-options page)
          (mapcar (lambda (x) (cons (format nil "~a" x) x))
                  (results-per-page-options page)))))


(defgeneric render-js (page)
  (:documentation "Generates any javascript this particular item-list may need"))

(defmethod render-js ((page item-list))
  "")

(defgeneric render-header (page)
  (:documentation "Renders the item-list's header"))

(defgeneric unoptimized-list-getter (page)
  (:documentation "Returns the complete list of items that could be displayed in this item-list.
    Called if use-optimized is false"))

(defgeneric optimized-list-count (page)
  (:documentation "Returns the total number of items available for display."))

(defgeneric optimized-list-getter (page offset count)
  (:documentation "Returns only those items actually being displayed, starting at offset"))

(defgeneric render-item (page item)
  (:documentation "Renders the given item"))


(defgeneric render-none (page)
  (:documentation "Called when there are no items to render in the list"))

(defmethod render-none ((page item-list))
  (when (not-blank (none-message page))
    (<:h2 :class "none-found" (<:as-html (none-message page)))))

(defaction do-results-per-page ((page item-list) count)
  (setf (results-per-page page) count)
  (setf (from page) 0))

(defmethod render-html ((page item-list))
  "Handles the basic rendering and page calculation for the item-list"
  (let* ((from (from page))
         (step (results-per-page page))
         (items (unless (use-optimized page)
                  (unoptimized-list-getter page)))
         (len (if (use-optimized page)
                  (optimized-list-count page)
                  (length items))))
    (declare (integer from step len))
    (<:table
     :class (div-class page) :id (div-id page) :cellspacing (cellspacing page)
     (when (zerop len)
       (render-none page))
     (let ((items (if (use-optimized page)
		      (optimized-list-getter page from step)
		      (nthcdr from items))))
       (do ((i 0 (1+ i))
	    (remainder (cdr items) (cdr remainder))
	    (item (car items) (car remainder)))
	   ((or (>= i step) (not item)))
	 (render-item page item)
         (setf (odd-row page) (not (odd-row page))))))
    (let* ((num-pages (ceiling len step))
	   (cur-page (floor from step)))
      (declare (integer num-pages cur-page))
      (when (> num-pages 1)
	(let ((page-index cur-page))
	  (<:ul
	   :class "nav-list"
	   (<ucw:form
	    :class "nav-page-form"
	    :action (navigate-to-page page (- page-index 1))
	    
	    (if (zerop cur-page)
		(progn
		  (<:li :class "first" 
			(<:a :class "nav-begin" (<:img :src "images/begin-dis.gif")))
		  (<:li (<:a :class "nav-back" 
			     (<:img :src "images/back-dis.gif"))))
		(progn
		  (<:li :class "first" (<ucw:a :class "nav-begin"
					       :action (navigate-to page 0)
					       :title "First Page"
					       (<:img :src "images/begin.gif")))
		  (<:li (<ucw:a :class "nav-back"
				:action (prev-items page)
				:title "Previous Page"
				(<:img :src "images/back.gif")))))
	    (<:li
	     :class "nav-page"
	     (render
	      (make-instance 'integer-spin-button
			     :value (1+ page-index)
			     :writer (lambda (x)
				       (setf page-index x))
			     :min-value 1 :max-value num-pages)))
	    (<:li
	     :class "nav-page page-count"
	     (<:ah "of " num-pages))
	    (if (equal cur-page (- num-pages 1))
		(progn
		  (<:li (<:a :class "nav-next" 
			     (<:img :src "images/next-dis.gif")))
		  (<:li (<:a :class "nav-end" 
			     (<:img :src "images/end-dis.gif"))))
		(progn
		  (<:li (<ucw:a :class "nav-next"
				:action (next-items page)
				:title "Next Page"
				(<:img :src "images/next.gif")))
		  (<:li (<ucw:a :class "nav-end"
				:action (navigate-to-page page (- num-pages 1))
				:title "Last Page"
				(<:img :src "images/end.gif"))))))
	   (dolist (results-per-page-count (reverse (results-per-page-options page)))
	     (let ((count (cdr results-per-page-count))
		   (title (car results-per-page-count)))
	       (<:li :class (if (= count 5) "num-results first-item" "num-results")
		     (if (= (results-per-page page) count)
			 (<:as-html title)
			 (<ucw:a :action (do-results-per-page page count)
				 :title "Results Per Page"
				 (<:as-html title))))))))))))

(defmethod render ((page item-list))
  ;; Javascript
  (<:script :type "text/javascript"
            (<:as-is
              (render-js page)))
  ;; Html
  (setf (odd-row page) t)
  (when (show-title page) (render-title page))
  (<:div 
   :class "box"
   (render-html page)))

(defaction next-items ((page item-list))
  (incf (from page) (results-per-page page)))

(defaction prev-items ((page item-list))
  (decf (from page)  (results-per-page page))
  (if (< (from page) 0)
      (setf (from page) 0)))

(defaction navigate-to ((page item-list) index)
  (setf (from page) index))

(defaction navigate-to-page ((comp item-list) page-number)
  (setf (from comp) (* page-number (results-per-page comp))))


(defgeneric navigate-to-item (item-list id)
  (:documentation "Jumps to the item with the given id"))



;;; filtered-item-list makes it easier to apply various filters to your item-list
;;; Each filter is associated with a key that identifies it (and makes it easy to replace).
;;; Use a construct like [and (get-filter-sql page) ...] in your query to apply these filters.

(defcomponent filtered-item-list (item-list)
  ((filters :accessor filters
            :type hash-table
            :initform (make-hash-table :test #'equal))
   (filter-tables :accessor filter-tables
                  :type hash-table
                  :initform (make-hash-table :test #'equal))
   (default-view-classes :accessor default-view-classes
     :type list
     :initarg :default-view-classes))
  (:documentation "item-list with support for filters"))

(defgeneric set-filter (filtered-item-list key sql table)
  (:documentation "Adds a filter, identified by key, to the item-list which uses sql to search view-class.
If a filter named key already exists, this replaces it"))

(defmethod set-filter ((page filtered-item-list) key sql table)
  (setf (gethash key (filters page)) sql)
  (setf (gethash key (filter-tables page)) table))


(defgeneric remove-filter (filtered-item-list key)
  (:documentation "Removes the filter identified by key from the item-list"))

(defmethod remove-filter ((page filtered-item-list) key)
  (remhash key (filters page))
  (remhash key (filter-tables page)))

(defgeneric get-filter-sql (filtered-item-list)
  (:documentation "Returns the sql needed to apply the various filters to this item-list"))

(defmethod get-filter-sql ((page filtered-item-list))
  (let ((sql-list (loop for v being the hash-value of (filters page)
                        collect v)))
    (if sql-list
        (apply #'clsql:sql-and sql-list)
        [= 1 1])))

(defgeneric get-filter-tables (filtered-item-list)
  (:documentation "Returns the tables which will need to be searched"))

(defmethod get-filter-tables ((page filtered-item-list))
  (remove-duplicates
    (append (default-view-classes page)
            (loop for c being the hash-value of (filter-tables page) collect c))))

#.(clsql:restore-sql-reader-syntax-state)
