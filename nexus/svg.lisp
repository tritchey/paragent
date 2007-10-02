#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

;;; Used to render svg charts. We have since replaced these with pngs, as svg is barely
;;;  supported in IE


(in-package :nexus)


;;; The base class for any png image

(defcomponent svg-component ()
  ((width :accessor width
          :initarg :width
          :initform 20)
   (height :accessor height
           :initarg :height
           :initform 20)))

(defmethod render :before ((img svg-component))
  (setf (get-header (context.response *context*) "Content-Type") "image/svg+xml"))

(defmethod render :wrapping ((img svg-component))
  (<svg:svg
    :xmlns "http://www.w3.org/2000/svg"
    :width (width img) :height (height img)
    (call-next-method)))

;;; Helper functions


(defun import-colors (&rest colors)
  (dolist (color colors)
    (case color
          ((:all)
           (import-colors :bg :high :medium :low :note))
          ((:all-line)
           (import-colors :bg-line :high-line :medium-line :low-line :note-line))
          ((:all-circle)
           (import-colors :bg-circle :high-circle :medium-circle :low-circle :note-circle))
          ((:bg :bg-line :bg-circle)
           (<svg:linear-gradient :id "bgfill"
				  :x1 "0%" :y1 "0%" :x2 "0%" :y2 "100%"
                                (<svg:stop :offset "5%" :stop-color "rgb(198,208,152)" :stop-opacity 1)
                                (<svg:stop :offset "95%" :stop-color "rgb(222,237,207)" :stop-opacity 1)))
          ((:high :high-line :high-circle)
             (<svg:linear-gradient :id "highfill"; :gradientUnits "userSpaceOnUse"
				  :x1 "0%" :y1 "0%" :x2 "0%" :y2 "100%"
                       (<svg:stop :offset "5%" :stop-color "rgb(229,91,23)" :stop-opacity 1)
                       (<svg:stop :offset "95%" :stop-color "rgb(199,61,0)" :stop-opacity 1)))
          ((:medium :medium-line :medium-circle)
           (<svg:linear-gradient :id "mediumfill" ; :gradientUnits "userSpaceOnUse"
				  :x1 "0%" :y1 "0%" :x2 "0%" :y2 "100%"
                                (<svg:stop :offset "5%" :stop-color "rgb(247,223,182)" :stop-opacity 1)
                                (<svg:stop :offset "95%" :stop-color "rgb(217,193,152)" :stop-opacity 1)))
          ((:low :low-line :low-circle)
           (<svg:linear-gradient :id "lowfill"; :gradientUnits "userSpaceOnUse"
				  :x1 "0%" :y1 "0%" :x2 "0%" :y2 "100%"
                                (<svg:stop :offset "5%" :stop-color "rgb(246,245,220)" :stop-opacity 1)
                                (<svg:stop :offset "95%" :stop-color "rgb(226,225,200)" :stop-opacity 1)))
          ((:note :note-line :note-circle)
           (<svg:linear-gradient :id "notefill"; :gradientUnits "userSpaceOnUse"
				  :x1 "0%" :y1 "0%" :x2 "0%" :y2 "100%"
                                (<svg:stop :offset "5%" :stop-color "rgb(176,208,214)" :stop-opacity 1)
                                (<svg:stop :offset "95%" :stop-color "rgb(146,178,184)" :stop-opacity 1))))))
          
           
(defun get-color (color)
  (case color
        (:high "url(#highfill)")
        (:medium "url(#mediumfill)")
        (:low "url(#lowfill)")
        (:note "url(#notefill)")
        (:bg "url(#bgfill)")
        (otherwise "black")))



;;; Pie chart

(defun <pie-chart (page slices)
  (<:embed :src (make-link (show-svg-pie-chart page slices)) :type "image/svg+xml"
           :height "200" :width "400")
  (when (internet-explorer-p)
    (<:p (<:ah "To see the above chart, you must either use "
               (<:a :href "http://www.mozilla.com/en-US/firefox/" "Firefox")
               " or install " 
               (<:a :href "http://www.adobe.com/svg/viewer/install/main.html"
                    "Adobe's SVG plugin.")))))

(defaction show-svg-pie-chart ((page paragent-component) slices)
  (call 'svg-pie-chart :slices slices))

(defcomponent svg-pie-chart (svg-component)
  ((slices :accessor slices
           :initarg :slices
           :initform '((0.3 :low "label 1") (0.2 :medium "label 2") (0.1 :high "label 3"))))
  (:default-initargs :width 400 :height 200))

(defmethod render ((img svg-pie-chart))
  (import-colors :all-circle)
  (<svg:g
    (let ((radius 75)
          (prev-x 100)
          (prev-y 175)
          (prev-percentage 0)
          (2pi (* 2 pi)))
      (<svg:circle :fill "url(#notefill)" :stroke "black"
                   :cx 100 :cy 100 :r radius)
      (dolist (slice (slices img))
        (let* ((cur-percentage (or (first slice) 0))
               (percentage (+ cur-percentage prev-percentage))
               (color (get-color (second slice)))
               (radians (* percentage 2pi))
               (next-x (+ 100 (* radius (sin radians))))
               (next-y (+ 100 (* radius (cos radians)))))
          (setf prev-percentage percentage)
          (<svg:path :fill color :stroke "white" :stroke-width 0.5
                     :d (format nil "M 100,100 L ~,5F,~,5F A ~,5F,~,5F 0 ~a,0 ~,5F,~,5F z"
                                prev-x prev-y radius radius
                                (if (> cur-percentage 0.5) 1 0)
                                 next-x next-y))
          (setf prev-x next-x)
          (setf prev-y next-y)))
      (<svg:circle :fill "none" :stroke "black"
                   :cx 100 :cy 100 :r radius)
      (let ((rect-y 10))
        (dolist (slice (slices img))
          (let ((color (get-color (second slice)))
                (label (third slice)))
            (when label
              (<svg:rect :x 200 :y rect-y :width 12 :height 12
                         :fill color :stroke "black" :stroke-width 0.5)

              (<svg:text :x 215 :y (+ rect-y 10) :fill "black" :font-size 12
                         (<:as-html label))
              (setf rect-y (+ rect-y 20))))))

    )))



;;; Line graph

(defun <line-graph (page data &key x-labels)
  (<:embed :src (make-link (show-svg-line-graph page data x-labels)) :type "image/svg+xml"
           :height "220" :width "500")
  (when (internet-explorer-p)
    (<:p (<:ah "To see the above graph, you must either use "
               (<:a :href "http://www.mozilla.com/en-US/firefox/" "Firefox")
               " or install " 
               (<:a :href "http://www.adobe.com/svg/viewer/install/main.html"
                    "Adobe's SVG plugin.")))))

(defaction show-svg-line-graph ((page paragent-component) data x-labels)
  (call 'svg-line-graph :data data :x-labels x-labels))

(defcomponent svg-line-graph (svg-component)
  ((data :accessor data
         :initarg :data
         :initform '((:high (3 3 3 4 3 2 3 3))
                     (:low (1 2 3 6 2 4 1 5))))
   (x-labels :accessor x-labels
             :initarg :x-labels
             :initform nil))
  (:default-initargs :width 500 :height 220))

(defun svg-path (points)
  (apply
    #'concatenate
    'string
    (let ((start (car points)))
      (cons (format nil "M ~f,~f" (first start) (second start))
            (mapcar
              (lambda (point)
                (case (third point)
                      ((:M) (format nil " M ~f,~f" (first point) (second point)))
                      ((:L nil otherwise) (format nil " L ~f,~f" (first point) (second point)))))
              (cdr points))))))

(defmethod render ((img svg-line-graph))
  (import-colors :all-line)
  (let* ((img-height 200)
         (x-labels (x-labels img))
         (max-value (apply #'max (mapcar
                                   (lambda (datum)
                                     (apply #'max (second datum)))
                                   (data img))))
         (scale-y (/ img-height (1+ max-value))))
    (<svg:g
      :transform "translate(0,200) scale(1,-1)"
      (<svg:rect :id "background" :fill "url(#bgfill)" :stroke "rgb(102,102,102)"
                 :width (width img) :height img-height)
      
      (dolist (datum (data img))
        (let* ((color (get-color (first datum)))
               (heights (second datum))
               (scale-x (/ (width img) (- (length heights) 1)))
               (x 0))
          (<svg:path :fill color :stroke "white" :stroke-width 4
                     :d (svg-path
                          (append
                            (cons (list 0 0)
                                  (mapcar
                                    (lambda (height)
                                      (prog1 (list x (* height scale-y))
                                             (incf x scale-x)))
                                    heights))
                            (list (list (- x scale-x) 0)))))))
      (<svg:rect :id "background" :fill "none" :stroke "rgb(102,102,102)"
                 :width (width img) :height img-height))
    (let ((length (- (length (or x-labels (second (car (data img))))) 1)))
      (let* ((scale-x (float (/ (width img) length)))
             (x 0))
        (dotimes (i length) ;Draw the unit lines
          (<svg:path :stroke "rgb(127,127,127)" :stroke-opacity 0.25
                     :d (svg-path
                          (list (list (* i scale-x) 0 :M)
                                (list (* i scale-x) img-height)))))
        (dolist (x-label x-labels)
          (<svg:text :x x :y 215 :fill "black" :font-size 12
                     (<:as-html x-label))
          (incf x scale-x))))
    ))
  


;;; Testing

(defaction show-svg-img-test ((page paragent-component))
  (call 'svg-img-test))

(defcomponent svg-img-test (svg-component)
  ()
  (:default-initargs :width 1000 :height 400))

(defmethod render ((img svg-img-test))
  (import-colors :all)
  (<svg:g
    :transform "translate(10,10)"
    (<svg:rect :id "background" :fill "url(#bgfill)" :stroke "rgb(102,102,102)"
               :width 601 :height 250)
    
    (<svg:path :stroke "none" :fill "url(#highfill)"
               :d "M 0 250 l 0 -50 l 100 -25 l 100 10 l 100 -50 l 100 -40 l 100 -75 l 100 175 L 600 250")
    (<svg:path :stroke "white" :stroke-width 4 :stroke-opacity 1 :fill "none"
               :d "M 0 200 l 100 -25 l 100 10 l 100 -50 l 100 -40 l 100 -75 l 100 175")
    
    (<svg:path :stroke "none" :fill "url(#mediumfill)"
               :d "M 0 250 l 0 -40 l 100 -15 l 100 0 l 100 -35 l 100 -40 l 100 -35 l 100 125 L 600 250")
    (<svg:path :stroke "white" :stroke-width 4 :stroke-opacity 1 :fill "none"
               :d "M 0 210 l 100 -15 l 100 0 l 100 -35 l 100 -40 l 100 -35 l 100 125")
    
    (<svg:path :stroke "none" :fill "url(#lowfill)"
               :d "M 0 250 l 0 -10 l 100 -25 l 100 10 l 100 -35 l 100 -10 l 100 -75 l 100 110 L 600 250")
    (<svg:path :stroke "white" :stroke-width 4 :stroke-opacity 1 :fill "none"
               :d "M 0 240 l 100 -25 l 100 10 l 100 -35 l 100 -10 l 100 -75 l 100 110")
    
    (<svg:path :stroke "none" :fill "url(#notefill)"
               :d "M 0 250 l 0 0 l 100 0 l 100 -10 l 100 -20 l 100 -20 l 100 40 l 100 0 L 600 250")
    (<svg:path :stroke "white" :stroke-width 4 :stroke-opacity 1 :fill "none"
               :d "M 0 250 l 0 0 l 100 0 l 100 -10 l 100 -20 l 100 -20 l 100 40 l 100 0")
    
    (<svg:rect :id "background" :fill "none" :stroke "rgb(102,102,102)"
               :width 601 :height 250)
    (<svg:path :stroke "rgb(127,127,127)" :stroke-opacity 0.25
               :d "M 100 0 l 0 250
               M 200 0 l 0 250
               M 300 0 l 0 250
               M 400 0 l 0 250
               M 500 0 l 0 250")))


(defentry-point "svg.ucw" (:application *my-app*)
  ()
  (call 'svg-demo))

(defcomponent svg-demo (paragent-component)
  ())

(defmethod render ((page svg-demo))
  (<:h1 "SVG")
  (<:p
    (<line-graph page '((:note (3 3 3 4 3 2 3 3))
                        (:high (1 2 3 6 2 4 1 5)))
                 :x-labels '("1" "second" "third" "fourth" "fifth" "sixth" "seventh" "eighth")
                 ))
  (<:p
    (<pie-chart page '((0.6 :high "label 3") (0.1 :medium "label 3") (0.2 :low "label 3"))))
  (<:embed :src (make-link (show-svg-img-test page)) :type "image/svg+xml" :height "600" :width "1000"))
