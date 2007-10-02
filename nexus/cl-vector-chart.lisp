;;;; Copyright (C) 2007  Frédéric Jolliton <frederic@jolliton.com>
;;;; http://projects.tuxee.net/cl-vectors/
;;;;
;;;; This file implement "pie" and "line" charts, using cl-vectors.
;;;;
;;;; It requires the Salza project from Xach Beane to write PNG image,
;;;; which can be found at:
;;;;
;;;;   http://www.cliki.net/Salza
;;;;
;;;; (Tested with version 0.7.4.)
;;;;
(defpackage :chart
  (:use :cl :aa :paths :vectors :salza :zpb-ttf :paths-ttf))

(in-package :chart)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *default-optimize* '(optimize (speed 3) (safety 0) (debug 0))))

(defvar *default-font* nil)

(deftype octet () '(unsigned-byte 8))

(deftype color-rgba () '(simple-array octet (4)))

(deftype bitmap-image () '(simple-array octet (* * 3)))

;;;--[ Bitmap image support ]---------------------------------------------------

(defun make-white-image (width height)
  "Create a buffer for a RGB image of WIDTH x HEIGHT dimension,
8BPP."
  (make-array (list height width 3)
              :element-type 'octet
              :initial-element 255))

(defun image-width (image)
  "Get the image width."
  (array-dimension image 1))

(defun image-height (image)
  "Get the image height."
  (array-dimension image 0))

(defun save-image-as-png (filename image)
  "Save IMAGE in PNG format into file with pathname FILENAME."
  (let* ((flat-data (make-array (apply #'* (array-dimensions image))
                                :element-type 'octet
                                :displaced-to image))
         (png (make-instance 'png:png
                             :color-type :truecolor
                             :width (image-width image)
                             :height (image-height image)
                             :image-data (copy-seq flat-data))))
    (png:write-png png filename)))

(defun write-png-to-stream (png stream)
  (png::write-png-header png stream)
  (png::write-ihdr png stream)
  (png::write-idat png stream)
  (png::write-iend png stream))

(defun image-to-png-stream (image stream)
  (let* ((flat-data (make-array (apply #'* (array-dimensions image))
                                :element-type 'octet
                                :displaced-to image))
         (png (make-instance 'png:png
                             :color-type :truecolor
                             :width (image-width image)
                             :height (image-height image)
                             :image-data (copy-seq flat-data))))
    (write-png-to-stream png stream)
    stream))

;;;--[ Rendering ]--------------------------------------------------------------

;;; NOTE: this may be counter-intuitive, but the alpha component is to
;;; be understood as follow: 0.0 is full transparency and 1.0 is full
;;; opacity.

;;; FIXME: opacity should be in range [0-256], not [0-255] !

(defun rgb (r g b &optional (a 1.0))
  "Create a new RGBA color object."
  ;; Component in the range 0.0-1.0
  (flet ((scale-component (c)
           (max 0 (min 255 (round (* 256 c))))))
    (let ((result (make-array 4 :element-type 'octet)))
      (setf (aref result 0) (scale-component r)
            (aref result 1) (scale-component g)
            (aref result 2) (scale-component b)
            (aref result 3) (scale-component a))
      result)))

(defun rgb8 (r g b &optional (a 255))
  "Create a new RGBA color object."
  ;; Component in the range 0-255
  (flet ((normalize (c)
           (max 0 (min 255 (round c)))))
    (let ((result (make-array 4 :element-type 'octet)))
      (setf (aref result 0) (normalize r)
            (aref result 1) (normalize g)
            (aref result 2) (normalize b)
            (aref result 3) (normalize a))
      result)))

(defun rgb-hexa (string)
  (assert (and (or (= 6 (length string))
                   (= 8 (length string)))
               (every (lambda (c) (digit-char-p c 16))
                      string))
          (string) "Expected a string of 6 or 8 hexa digits.")
  (let ((r (parse-integer (subseq string 0 2) :radix 16))
        (g (parse-integer (subseq string 2 4) :radix 16))
        (b (parse-integer (subseq string 4 6) :radix 16))
        (a (if (>= (length string) 8)
               (parse-integer (subseq string 6 8) :radix 16)
               255)))
    (rgb (/ r 255.0)
         (/ g 255.0)
         (/ b 255.0)
         (/ a 255.0))))

(defun special-rgb (color)
  (list :special-rgb color))

(defun linear-gradient (x1 y1 x2 y2 color1 color2)
  (list :linear-gradient x1 y1 x2 y2 color1 color2))

(defun radial-gradient (x y radius color1 color2)
  (list :radial-gradient x y radius color1 color2))

(declaim (inline put-pixel))
(defun put-pixel (image x y color alpha)
  (declare #.*default-optimize*)
  (declare (color-rgba color)
           (bitmap-image image)
           ((integer 0 256) alpha))
  #+nil(assert (<= 0 alpha 256))
  (let ((inv-alpha (- 256 alpha)))
    (setf (aref image y x 0) (floor (+ (* inv-alpha (aref image y x 0))
                                       (* alpha (aref color 0)))
                                    256)
          (aref image y x 1) (floor (+ (* inv-alpha (aref image y x 1))
                                       (* alpha (aref color 1)))
                                    256)
          (aref image y x 2) (floor (+ (* inv-alpha (aref image y x 2))
                                       (* alpha (aref color 2)))
                                    256))
    nil))

(defun render-solid (image state color)
  "Render the rasterizer STATE into the bitmap IMAGE with COLOR."
  (declare #.*default-optimize*)
  (declare (bitmap-image image)
           (color-rgba color))
  (let ((width-1 (1- (image-width image)))
        (height-1 (1- (image-height image)))
        (opacity (aref color 3)))
    (assert (<= 0 opacity 256))
    (cells-sweep state
                 (lambda (x y alpha)
                   (when (and (<= 0 x width-1)
                              (<= 0 y height-1))
                     (put-pixel image x y color
                                (floor (* opacity (min 256 (abs alpha))) 256))))
                 (lambda (x1 x2 y alpha)
                   (when (and (<= x1 width-1)
                              (<= 0 x2)
                              (<= 0 y height-1))
                     (loop with alpha-span = (floor (* opacity (min 256 (abs alpha))) 256)
                        for x from (max 0 x1) upto (min width-1 (1- x2))
                        do (put-pixel image x y color alpha-span)))))
    nil))

(defun render-solid-special (image state color)
  "Render the rasterizer STATE into the bitmap IMAGE with COLOR,
but only for the \"positive\" regions of the AA state."
  (declare #.*default-optimize*)
  (declare (bitmap-image image)
           (color-rgba color))
  (let ((width-1 (1- (image-width image)))
        (height-1 (1- (image-height image)))
        (opacity (aref color 3)))
    (assert (<= 0 opacity 256))
    (cells-sweep state
                 (lambda (x y alpha)
                   (when (and (<= 0 x width-1)
                              (<= 0 y height-1))
                     (put-pixel image x y color
                                (floor (* opacity (min 256 (max 0 alpha))) 256))))
                 (lambda (x1 x2 y alpha)
                   (when (and (<= x1 width-1)
                              (<= 0 x2)
                              (<= 0 y height-1))
                     (loop with alpha-span = (floor (* opacity (min 256 (max 0 alpha))) 256)
                        for x from (max 0 x1) upto (min width-1 (1- x2))
                        do (put-pixel image x y color alpha-span)))))
    nil))

(defun render-linear-gradient (image state x1 y1 x2 y2 color1 color2)
  "Render the rasterizer STATE into the bitmap IMAGE with COLOR."
  (declare #.*default-optimize*)
  (declare (bitmap-image image)
           (color-rgba color1 color2))
  (let* ((width-1 (1- (image-width image)))
         (height-1 (1- (image-height image)))
         (pixel-color (rgb 0 0 0 1)))
    (flet ((blend-color (a)
             (cond
               ((<= a 0.0)
                color1)
               ((>= a 1.0)
                color2)
               (t
                (loop for rgb below 4
                   do (setf (aref pixel-color rgb)
                            (+ (aref color1 rgb)
                               (round (* a (- (aref color2 rgb)
                                              (aref color1 rgb)))))))
                pixel-color))))
      (multiple-value-bind (min-x min-y max-x max-y) (state-bounding-box state)
        (when min-x
          (let* ((dx (- x2 x1))
                 (dy (- y2 y1))
                 (l (+ (expt dx 2)
                       (expt dy 2)))
                 (nx (/ dx l))
                 (ny (/ dy l))
                 (fx (1+ (- max-x min-x)))
                 (fy (1+ (- max-y min-y)))
                 (a (/ nx fx))
                 (b (/ ny fy))
                 (cx (- (* nx (+ (/ min-x fx) x1))))
                 (cy (- (* ny (+ (/ min-y fy) y1))))
                 (c (+ cx cy)))
            (cells-sweep state
                         (lambda (x y alpha)
                           (when (and (<= 0 x width-1)
                                      (<= 0 y height-1))
                             (let* ((m (+ (* x a) (* y b) c))
                                    (color (blend-color m)))
                               (put-pixel image x y color
                                          (floor (* (aref color 3) (min 256 (abs alpha))) 256))))))))))
    nil))

(defun render-radial-gradient (image state xc yc radius color1 color2)
  "Render the rasterizer STATE into the bitmap IMAGE with COLOR."
  (declare #.*default-optimize*)
  (declare (bitmap-image image)
           (color-rgba color1 color2))
  (let* ((width-1 (1- (image-width image)))
         (height-1 (1- (image-height image)))
         (pixel-color (rgb 0 0 0 1)))
    (flet ((blend-color (a)
             (cond
               ((<= a 0.0)
                color1)
               ((>= a 1.0)
                color2)
               (t
                (loop for rgb below 4
                   do (setf (aref pixel-color rgb)
                            (+ (aref color1 rgb)
                               (round (* a (- (aref color2 rgb)
                                              (aref color1 rgb)))))))
                pixel-color))))
      (multiple-value-bind (min-x min-y max-x max-y) (state-bounding-box state)
        (when min-x
          ;; To compute 'a', we first calculate the position of the
          ;; point (x,y) in the bounding box area as (x',y').
          ;;
          ;; x' = (x-minx)/(maxx-minx+1)
          ;; y' = (y-miny)/(maxy-miny+1)
          ;;
          ;; Then we compute the square distance between (x',y') and
          ;; (xc,yc) with:
          ;;
          ;; sqrt[(x'-xc)^2+(y'-yc)^2]
          ;;
          ;; that we further divide by radius to get a normalized
          ;; value between 0 and 1 when the projected point lie
          ;; between into the circle at (cx,cy) of the given radius.
          ;;
          ;; The whole computation can be simplified to:
          ;; sqrt[(a*x+cx)^2+(b*y+cy)^2]
          ;;
          ;; with:
          ;; a' = 1/(maxx-minx+1)
          ;; b' = 1/(maxy-miny+1)
          ;; cx' = -(a'*minx+xc)
          ;; cy' = -(b'*miny+yc)
          ;;
          ;; a = a'/radius
          ;; b = b'/radius
          ;; cx = cx'/radius
          ;; cy = cy'/radius
          (let* ((a (/ (* radius (1+ (- max-x min-x)))))
                 (b (/ (* radius (1+ (- max-y min-y)))))
                 (cx (- (+ (* min-x a) (/ xc radius))))
                 (cy (- (+ (* min-y b) (/ yc radius)))))
            (cells-sweep state
                         (lambda (x y alpha)
                           (when (and (<= 0 x width-1)
                                      (<= 0 y height-1))
                             (let* ((m (sqrt (+ (expt (+ (* x a) cx) 2)
                                                (expt (+ (* y b) cy) 2))))
                                    (color (blend-color m)))
                               (put-pixel image x y color
                                          (floor (* (aref color 3) (min 256 (abs alpha))) 256))))))))))
    nil))

(defun render (image state fill-style)
  (if (arrayp fill-style)
      (render-solid image state fill-style)
      (ecase (first fill-style)
        (:special-rgb
         (apply #'render-solid-special image state (rest fill-style)))
        (:linear-gradient
         (apply #'render-linear-gradient image state (rest fill-style)))
        (:radial-gradient
         (apply #'render-radial-gradient image state (rest fill-style)))))
  nil)

(defun render-state (image state fill-style)
  "Render the PATHS into IMAGE. All the paths are rendered with a
single state."
  (render image state fill-style)
  nil)

(defun render-paths (image paths fill-style)
  "Render the PATHS into IMAGE. All the paths are rendered with a
single state."
  (let ((state (make-state)))
    (dolist (path (if (listp paths) paths (list paths)))
      (update-state state path))
    (render image state fill-style))
  nil)

(defun freeze-graph (graph)
  "Create a graph with all paths transformed to AA state."
  (loop for (fill-style . paths) in graph
     collect (let ((state (make-state)))
               (dolist (path (if (listp paths) paths (list paths)))
                 (update-state state path))
               (cons fill-style state))))

(defun state-bounding-box (state &optional min-x min-y max-x max-y)
  "Compute the bounding box for the whole graph in
FREEZED-GRAPH. Return values MIN-X MIN-Y MAX-X MAX-Y."
  (cells-sweep state
               (lambda (x y alpha)
                 (declare (ignore alpha))
                 (if min-x
                     (setf min-x (min min-x x)
                           max-x (max max-x x)
                           min-y (min min-y y)
                           max-y (max max-y y))
                     (setf min-x x
                           max-x x
                           min-y y
                           max-y y)))
               (lambda (x1 x2 y alpha)
                 (declare (ignore alpha))
                 (if min-x
                     (setf min-x (min min-x x1)
                           max-x (max max-x x2)
                           min-y (min min-y y)
                           max-y (max max-y y))
                     (setf min-x x1
                           max-x x2
                           min-y y
                           max-y y))))
  (values min-x min-y max-x max-y))

(defun freezed-graph-bounding-box (freezed-graph &optional min-x min-y max-x max-y)
  "Compute the bounding box for the whole graph in
FREEZED-GRAPH. Return values MIN-X MIN-Y MAX-X MAX-Y."
  (loop for (fill-style . state) in freezed-graph
     do (setf (values min-x min-y max-x max-y)
              (state-bounding-box state min-x min-y max-x max-y)))
  (values min-x min-y max-x max-y))

(defun render-graph (image graph)
  "Render GRAPH (which may be a \"freezed\" graph) to IMAGE."
  (loop for (fill-style . paths) in graph
     do (if (typep paths 'aa::state)
            (render-state image paths fill-style)
            (render-paths image paths fill-style)))
  nil)

(defun render-graph-to-file (filename graph)
  (let* ((freezed-graph (freeze-graph graph))
         (bounding-box (multiple-value-list (freezed-graph-bounding-box freezed-graph)))
         (image (make-white-image (1+ (or (third bounding-box) 0))
                                  (1+ (or (fourth bounding-box) 0)))))
    (time (render-graph image graph))
    (save-image-as-png filename image))
  nil)

(defun render-graph-to-png-stream (stream graph)
  (let* ((freezed-graph (freeze-graph graph))
         (bounding-box (multiple-value-list (freezed-graph-bounding-box freezed-graph)))
         (image (make-white-image (1+ (or (third bounding-box) 0))
                                  (1+ (or (fourth bounding-box) 0)))))
    (render-graph image graph)
    (image-to-png-stream image stream))
  nil)

;;;--[ Pie graph ]--------------------------------------------------------------

(defun make-sector-path (cx cy radius start-angle end-angle)
  (let ((path (create-path :polygon))
        (p1 (point-rotate (make-point radius 0) start-angle))
        (p2 (point-rotate (make-point radius 0) end-angle)))
    (path-extend path (make-straight-line) p2)
    (path-extend path (make-arc radius radius
                                :large-arc-flag (> (mod (- end-angle start-angle)
                                                        (* 2 pi))
                                                   pi))
                      p1)
    (path-extend path (make-straight-line) (make-point 0 0))
    (path-translate path (make-point cx cy))
    path))

(defun make-pie-graph (data &key total (radius 180) (label-size 12) background)
  "Generate a pie graph.

DATA is a list of DATA-ITEM. DATA-ITEM is a list of 3 elements:
COLOR, LABEL and VALUE.

Example:

  (make-pie-graph (list (list (rgb 1 0 0) \"Jan\" 27.0)
                        (list (rgb 0 1 0) \"Feb\" 32.0)
                        (list (rgb 0 0 1) \"Mar\" 19.0))
                  :total 100.0)

If TOTAL is unspecified or false, it will default to the sum of
value in DATA.
"
  (unless total
    (setq total (loop for (color label value) in data
                   summing value)))
  (unless background
    (setq background (linear-gradient 0 0 0 1
                                      (rgb-hexa "b0d0d8")
                                      (rgb-hexa "94b3ba"))))
  (let* ((cx (+ 2 radius))
         (cy (+ 2 radius))
         (circle-thickness (/ radius 30.0))
         (label-x (* 2.6 radius))
         (label-y-start 20)
         (label-y-spacing (* 3.5 label-size))
         ;; state
         graph
         (font-scale (/ label-size 1024.0)))
    ;; the background circle
    (push (cons background (make-circle-path cx cy radius)) graph)
    (loop with angle = (/ pi 2.0)
       with label-y = label-y-start
       for (color label value) in data
       do
       ;; the sector
          (unless (equal value 0.0)
            (let* ((end-angle (- angle (* 2 pi (/ value total))))
                   (sector (make-sector-path cx cy (+ 4 radius) end-angle angle)))
              (push (cons color sector) graph)
              (push (cons (rgb 1 1 1) (stroke-path sector 4.0 :joint :round
                                                   :assume-type :closed-polyline))
                    graph)
              (setf angle end-angle)))
       ;; the label
       (let ((box (make-rectangle-path/center label-x label-y
                                              (* 1.25 label-size) (* 1.25 label-size)
                                              :round (/ label-size 3.0)))
             (box-fill (make-rectangle-path/center label-x label-y
                                                   label-size label-size)))
         ;; the label box
         (push (cons (rgb .8 .8 .8) box) graph)
         (push (cons color box-fill) graph)
         ;; the label string
         (push (cons (rgb 0.4 0.4 0.4)
                     (paths-ttf:paths-from-string
                      *default-font* label
                      :offset (make-point (+ label-x (* 1.5 label-size))
                                          (+ label-y (* 0.75 label-size)))
                      :scale-x font-scale
                      :scale-y (- font-scale)))
               graph)
         (incf label-y label-y-spacing)))
    ;; the circle border
    (push (list (rgb 0.4 0.4 0.4)
                (make-circle-path cx cy (+ radius circle-thickness))
                (path-reversed (make-circle-path cx cy radius)))
          graph)
    ;; the bump effect
;    (push (list (special-rgb (rgb 1 1 1 0.35))
;                (make-circle-path cx cy (1+ radius))
;                (path-reversed (make-circle-path (+ cx 4) (+ cy 4) (+ 3 radius))))
;          graph)
;    (push (list (special-rgb (rgb 0 0 0 0.35))
;                (make-circle-path cx cy (1+ radius))
;                (path-reversed (make-circle-path (- cx 4) (- cy 4) (+ 3 radius))))
;          graph)
    (nreverse graph)))

;;;--[ Time graph ]-------------------------------------------------------------

(defun make-time-graph (labels data
                         &key (width 600) (height 400) (label-size 8)
                         max-y background)
  "Generate a time graph.

LABELS is a list of string. DATA is a list DATA-ITEM. DATA-ITEM
is a list whose first element is the color or gradient to use to
fill the corresponding area, and the second element is a list of
values (which should contains (1+ (length labels)) element.)

Example:

  (make-time-graph (list \"Jan\" \"Feb\" \"Mar\")
                   (list (list (rgb 1 0 0) (list 1 5 3 4))
                         (list (rgb 0 1 0) (list 2 0 -2 1))
                         (list (rgb 0 0 1) (list 6 5 3 1))))

If MAX-Y is unspecified or false, it will default to the maximum
value found in the data."
  (unless max-y
    (loop for (color values) in data
       do (let ((max (reduce #'max values)))
            (setf max-y (if max-y (max max-y max) max)))))
  (when (zerop max-y)
    (setf max-y 400))
  (unless background
    (setq background (rgb 1 1 1)))
  (let* ((start-x 2)
         (start-y 2)
         (x-spacing (/ width (length labels)))
         (scale-y (/ height max-y))
         (height (+ start-y (* max-y scale-y)))
         ;;
         (font-scale (/ label-size 1024.0))
         (size (length labels))
         graph)
    (flet ((index-to-x (index)
             (+ start-x (* x-spacing index)))
           (value-to-y (value)
             (- height (* value scale-y))))
      (let ((clip-zone (make-rectangle-path (index-to-x 0) (value-to-y 0)
                                            (index-to-x size) (value-to-y max-y))))
        ;; The frame and the background of the graph
        (push (list (rgb 0 0 0) (stroke-path clip-zone 2.0 :joint :miter)) graph)
        (push (list background clip-zone)
              graph)
        ;; Area for each set of value
        (loop for (color values) in (reverse data)
           do (let ((area (create-path :polygon)))
                (path-extend area (make-straight-line)
                             (make-point (index-to-x 0) (value-to-y 0)))
                (loop for value in values
                   for i from 0
                   do (path-extend area (make-straight-line)
                                   (make-point (index-to-x i) (value-to-y value)))
                   finally (path-extend area (make-straight-line)
                                        (make-point (index-to-x i) (value-to-y 0))))
                (push (cons (rgb 1 1 1) (clip-path/path
                                         (stroke-path
                                          (path-reversed area)
                                          5 :joint :miter :inner-joint :miter)
                                         clip-zone))
                      graph)
                (push (cons color (clip-path/path area clip-zone)) graph)))
        ;; Labels and vertical lines
        (loop for label in labels
           for index from 0
           for x = (index-to-x index)
           for first = t then nil
           do (push (cons (rgb 0 0 0)
                          (paths-ttf:paths-from-string
                           *default-font* label
                           :offset (make-point x (+ height (* label-size 2.1)))
                           :scale-x font-scale
                           :scale-y (- font-scale)))
                    graph)
           (unless first
             (push (cons (rgb 0 0 0 0.08)
                         (make-rectangle-path (1- x) (value-to-y 0)
                                              (1+ x) (value-to-y max-y)))
                   graph)))
        ;; The bump effect
;        (push (list (rgb 0 0 0 0.25)
;                    (make-rectangle-path (index-to-x 0) (value-to-y 0)
;                                         (index-to-x size) (- (value-to-y 0) 2))
;                    (make-rectangle-path (index-to-x size) (value-to-y 0)
;                                         (- (index-to-x size) 2) (value-to-y max-y)))
;              graph)
;        (push (list (rgb 1 1 1 0.5)
;                    (make-rectangle-path (index-to-x 0) (value-to-y 0)
;                                         (+ 2 (index-to-x 0)) (value-to-y max-y))
;                    (make-rectangle-path (index-to-x 0) (value-to-y max-y)
;                                         (index-to-x size) (+ (value-to-y max-y) 2)))
;              graph)
))
    (nreverse graph)))

;;;--[ Tests ]------------------------------------------------------------------

(defun test-bounding-box ()
  (let* ((graph (make-pie-graph (list (list (rgb 1 .4 0) "Expired" 5.0)
                                      (list (rgb 1 .8 .8) "Expiring this month" 3.0))
                                :total 40.0))
         (freezed-graph (freeze-graph graph)))
    (print (multiple-value-list (freezed-graph-bounding-box freezed-graph)))))

(defun test-png ()
  (let ((image (make-white-image 800 600)))
    (loop for y from 20 below 580
       do (loop for x from 20 below 780
             do (loop for rgb below 3
                   do (setf (aref image y x rgb) 0))))
    (save-image-as-png "foo.png" image)))

(defun test-gradient ()
  (render-graph-to-file "gradient-linear.png"
                        (list (cons (linear-gradient 0 0 1 1 (rgb 0 0 0) (rgb 1 1 1))
                                    (make-rectangle-path 10 10 100 100))
                              (cons (linear-gradient 0 0 1 0 (rgb 0 0 0) (rgb 1 1 1))
                                    (make-rectangle-path 110 10 200 100))
                              (cons (linear-gradient 0 0 0 1 (rgb 0 0 0) (rgb 1 1 1))
                                    (make-rectangle-path 10 110 100 200))
                              (cons (linear-gradient .25 .75 .75 .25 (rgb 0 0 0) (rgb 1 1 1))
                                    (make-rectangle-path 110 110 200 200))
                              (cons (linear-gradient .25 .25 .75 .75 (rgb 0 0 0) (rgb 1 1 1))
                                    (make-rectangle-path 110 210 200 300))))
  (render-graph-to-file "gradient-radial.png"
                        (list (cons (radial-gradient .5 .5 .5 (rgb 0 0 0) (rgb 1 1 1))
                                    (make-rectangle-path 10 10 100 100))
                              (cons (radial-gradient 1 1 .5 (rgb 0 0 0) (rgb 1 1 1))
                                    (make-rectangle-path 110 10 200 100))
                              (cons (radial-gradient 0 0 1 (rgb 0 0 0) (rgb 1 1 1))
                                    (make-rectangle-path 10 110 100 200))
                              (cons (radial-gradient 1 1 (sqrt 2) (rgb 0 0 0) (rgb 1 1 1))
                                    (make-rectangle-path 110 110 200 200)))))

(defun test-png-stream ()
  (with-open-file (stream "foo.png" :direction :output :element-type 'octet :if-exists :supersede)
    (render-graph-to-png-stream stream (make-pie-graph ()))))

;;;-----------------------------------------------------------------------------

;;; This function is a counterpart of nexus:import-colors,
;;; nexus:get-color. Maybe :)
(defun get-color (color)
  (case color
    ((:bg :bg-line :bg-circle)
     (linear-gradient 0.0 0.0 0.0 1.0 (rgb8 222 237 207) (rgb8 198 208 152)))
    ((:high :high-line :high-circle)
     (linear-gradient 0.0 0.0 0.0 1.0 (rgb8 229 91 23) (rgb8 199 61 0)))
    ((:medium :medium-line :medium-circle)
     (linear-gradient 0.0 0.0 0.0 1.0 (rgb8 247 223 182) (rgb8 217 193 152)))
    ((:low :low-line :low-circle)
     (linear-gradient 0.0 0.0 0.0 1.0 (rgb8 246 245 220) (rgb8 226 225 200)))
    ((:note :note-line :note-circle)
     (linear-gradient 0.0 0.0 0.0 1.0 (rgb8 176 208 214) (rgb8 146 178 184)))
    (t
     (rgb 0 0 0))))

(defun chart-test ()
  (unless *default-font*
    (setq *default-font* (zpb-ttf:open-font-loader "/usr/share/fonts/webcore/verdanab.ttf")))
  ;; Pie graph
  (render-graph-to-file "pie-example.png"
                        (make-pie-graph `((,(get-color :high-circle) "Expired" 5.0)
                                          (,(get-color :medium-circle) "Expiring this month" 3.0))
                                        :background (get-color :note-circle)
                                        :total 40.0
                                        :radius 100
                                        :label-size 6))
  ;; Time graph
  (render-graph-to-file "graph-example.png"
                        (make-time-graph '("Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
                                         `((,(get-color :note) (100.0 30.0 0.0 0.0 0.0 10.0 15.0))
                                           (,(get-color :low) (50.0 40.0 -10.0 20.0 70.0 0.0 0.0)))
                                         :background (get-color :bg-line)
                                         :width 500
                                         :height 200
                                         :label-size 5))
  (render-graph-to-file "graph2-example.png"
                        (make-time-graph '("A" "B" "C" "D" "A" "B" "C" "D"
                                           "A" "B" "C" "D" "A" "B" "C" "D"
                                           "A" "B" "C" "D" "A" "B" "C" "D")
                                         `((,(get-color :note) ,(loop repeat 25 collect (random 100)))
                                           (,(get-color :low) ,(loop repeat 25 collect (random 100))))
                                         :background (get-color :bg-line)
                                         :width 500
                                         :height 200
                                         :label-size 5)))
