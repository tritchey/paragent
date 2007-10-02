;;;; Created on 2006-09-07 16:19:25

(in-package :nexus)

(defclass content-type-component-mixin ()
  ((content-type :initarg :content-type :accessor content-type)))

(defgeneric render-loop (component)
  (:documentation "Keeps trying to render the component until it succeeds"))

(defmethod render-loop ((comp content-type-component-mixin))
  (restart-case
      (render comp)
    (retry ()
      :report "Retry calling RENDER."
      (return-from render-loop (render-loop comp)))))

(defmethod ucw::render :around ((comp content-type-component-mixin))
  (setf (get-header (context.response *context*) "Content-Type")
        (content-type comp))
  (send-headers (context.response *context*))
  (call-next-method))

;;; The base class for any png image
(defcomponent png-component (content-type-component-mixin)
  ((width :accessor width
          :initarg :width
          :type integer
          :initform 20)
   (height :accessor height
           :initarg :height
           :type integer
           :initform 20))
  (:default-initargs :content-type "image/png"))

(defgeneric render-image (img)
  (:documentation "Renders the image"))

(defmethod render ((img png-component))
  (let* ((response (ucw:context.response *context*))
         (stream (ucw::network-stream response)))
      (chart::render-graph-to-png-stream stream (render-image img))))

;;; Pie chart class

(defmacro <pie-chart (page slices &key radius)
  `(<:img :src (make-link (show-pie-chart ,page ,slices :radius ,radius))))

(defaction show-pie-chart ((page paragent-component) slices &key radius)
  (call 'img-pie-chart :slices (remove nil slices) :radius (or radius 100)))

(defcomponent img-pie-chart (png-component)
  ((slices :accessor slices
           :initarg :slices
           :initform nil)
   (radius :accessor radius
           :initarg :radius
           :type (integer 0 *)
           :initform 100)))

(defmethod render-image ((img img-pie-chart))
  (let (( chart::*default-font* (zpb-ttf:open-font-loader 
				 "/lisp/repos/nexus/verdanab.ttf")))
    (let ((ret nil))
      (dolist (slice (slices img))
        (unless ret
          (when (equal (third slice) 1)
            (setf ret (chart::make-pie-graph 
                       (mapcar (lambda (slice)
                                 (list (first slice) (second slice) 0.0))
                               (slices img))
                       :background (first slice)
                       :total 1.0
                       :radius (radius img)
                       :label-size 6)))))
      (unless ret
        (setf ret (chart::make-pie-graph (slices img)
                                         :background (chart::get-color :note-circle)
                                         :total 1.0
                                         :radius (radius img)
                                         :label-size 6)))
      ret)))

;;; Line graph

(defmacro <line-graph (page x-labels data &key (color :black))
  `(<:img :src (make-link (show-line-graph ,page ,x-labels ,data :color ,color))))

(defaction show-line-graph ((page paragent-component) x-labels data 
                            &key (height 200) (width 500))
  (call-component nil (make-instance 'img-line-graph :x-labels x-labels :data data :height height :width width)))

(defcomponent img-line-graph (png-component)
  ((x-labels :accessor x-labels
	     :initarg :x-labels
	     :initform nil)
   (data :accessor data
         :initarg :data
         :initform nil))
  (:default-initargs
   :height 200 :width 500))

(defmethod render-image ((img img-line-graph))
  (let (( chart::*default-font* (zpb-ttf:open-font-loader 
				 "/lisp/repos/nexus/verdanab.ttf")))
    (when (not (data img))
      (setf (data img) (cons (chart::get-color :note) 
			     (make-list (length (x-labels img)) :initial-element 1))))

    (chart::make-time-graph (x-labels img)
			    (data img)
			    :background (chart::get-color :bg-line)
			    :width (width img)
			    :height (height img)
			    :label-size 5)))