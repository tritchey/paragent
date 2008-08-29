;;;; worm-chart.lisp

(in-package #:wormtrails)

(defclass client-chart (chart) ()
  (:default-initargs))

(defclass day-bucket (bucket) ())

(defclass client-sample (sample) ())

(defclass client-worm (thing) ())

(defmethod create-worm (name (chart client-chart))
  (make-instance 'client-worm :name name))

(defmethod create-bucket (index (chart client-chart))
  (make-instance 'day-bucket :index index))

(defmethod create-sample (worm (day-bucket day-bucket))
  (make-instance 'client-sample :thing worm :bucket day-bucket))

(defmethod chart-label ((day-bucket day-bucket))
  (let ((day-bucket (index day-bucket)))
    (cond ((= day-bucket 0)
           "SUNDAY")
	  ((= day-bucket 1)
	   "MONDAY")
	  ((= day-bucket 2)
	   "TUESDAY")
	  ((= day-bucket 3)
	   "WEDNESDAY")
	  ((= day-bucket 4)
	   "THURSDAY")
	  ((= day-bucket 5)
	   "FRIDAY")
	  ((= day-bucket 6)
	   "SATURDAY"))))


(defun shade (start string end)
  (let ((h1 (hash1 string))
        (h2 (hash2 string)))
    (hsv-color (hash-range start h1 end)
               (hash-range 0.7 h2 1.0)
               (hash-range 0.3 h2 1.0))))

(defun red-shade (string)
  (shade 5 string 20))

(defun blue-shade (string)
  (shade 5 string 20))

(defun green-shade (string)
  (shade 5 string 20))

(defun maximum (list &key (key 'identity) (test '<))
  (when list
    (let* ((max (first list))
           (maxkey (funcall key max)))
      (dolist (elt (rest list) max)
        (let ((test-key (funcall key elt) ))
          (when (funcall test maxkey test-key)
            (setf max elt
                  maxkey test-key)))))))

(defmethod highest-activity ((client-worm thing))
  (maximum (samples client-worm) :key 'value))

(defun total-activity (client-worm)
  (reduce #'+ (samples client-worm)))

(defun client-color (clientname active-day-bucket)
  (cond ((<= 7 active-day-bucket 14)
         (blue-shade clientname))
        ((<= 15 active-day-bucket 22)
         (green-shade clientname))
        (t
         (red-shade clientname))))

(defmethod best-label-sample ((client-worm thing))
  (highest-activity client-worm))

(defmethod establish-colors ((chart client-chart))
  (dolist (client-worm (only-top-things chart))
    (let ((sample (highest-activity client-worm)))
      (when sample
        (let ((bucket (bucket sample)))
          (setf (color client-worm)
                (client-color (name client-worm) (index bucket))))))))

(defun client-chart (stream)
  (let ((chart (make-instance 'client-chart))
        (scale 5.0)
	(wormtrails::*font-file* "/Users/tritchey/Projects/Paragent/git/nexus/font.ttf"))
    (wormtrails::add-data chart 0 "Bohannon" 1)
    (wormtrails::add-data chart 0 "McGill" 3)
    (wormtrails::add-data chart 0 "Paragent" 5)
    (wormtrails::add-data chart 0 "Bitfauna" 5)
    (wormtrails::add-data chart 0 "Musal DDS" 7)
    (wormtrails::add-data chart 0 "Hiott Engineering" 3)
    (wormtrails::add-data chart 1 "Bohannon" 2)
    (wormtrails::add-data chart 1 "McGill" 2)
    (wormtrails::add-data chart 1 "Paragent" 4)
    (wormtrails::add-data chart 1 "Bitfauna" 6)
    (wormtrails::add-data chart 1 "Musal DDS" 7)
    (wormtrails::add-data chart 1 "Hiott Engineering" 1)
    (wormtrails::add-data chart 2 "Bohannon" 4)
    (wormtrails::add-data chart 2 "McGill" 6)
    (wormtrails::add-data chart 2 "Paragent" 5)
    (wormtrails::add-data chart 2 "Bitfauna" 7)
    (wormtrails::add-data chart 2 "Musal DDS" 7)
    (wormtrails::add-data chart 2 "Hiott Engineering" 2)
    (wormtrails::add-data chart 3 "Bohannon" 2)
    (wormtrails::add-data chart 3 "McGill" 6)
    (wormtrails::add-data chart 3 "Paragent" 5)
    (wormtrails::add-data chart 3 "Bitfauna" 10)
    (wormtrails::add-data chart 3 "Musal DDS" 7)
    (wormtrails::add-data chart 3 "Hiott Engineering" 4)
    (wormtrails::add-data chart 4 "Bohannon" 1)
    (wormtrails::add-data chart 4 "McGill" 2)
    (wormtrails::add-data chart 4 "Paragent" 5)
    (wormtrails::add-data chart 4 "Bitfauna" 20)
    (wormtrails::add-data chart 4 "Musal DDS" 10)
    (wormtrails::add-data chart 4 "Hiott Engineering" 9)
    (wormtrails::add-data chart 5 "Bohannon" 1)
    (wormtrails::add-data chart 5 "McGill" 1)
    (wormtrails::add-data chart 5 "Paragent" 1)
    (wormtrails::add-data chart 5 "Bitfauna" 22)
    (wormtrails::add-data chart 5 "Musal DDS" 14)
    (wormtrails::add-data chart 5 "Hiott Engineering" 5)
    (wormtrails::add-data chart 6 "Bohannon" 1)
    (wormtrails::add-data chart 6 "McGill" 1)
    (wormtrails::add-data chart 6 "Paragent" 2)
    (wormtrails::add-data chart 6 "Bitfauna" 19)
    (wormtrails::add-data chart 6 "Musal DDS" 7)
    (wormtrails::add-data chart 6 "Hiott Engineering" 2)
    (wormtrails::output-png-stream chart stream
                 :scaler (wormtrails::linear-scaler scale)
                 :metric-height 20 :metric-label "10 EVENTS")))

(in-package :nexus)

(defmacro <wormtrail-chart (page)
  `(<:img :src (make-link (show-wormtrail-chart ,page))))

(defaction show-wormtrail-chart ((page paragent-component))
  (call 'img-wormtrail-chart))

(defcomponent img-wormtrail-chart (png-component) ())

(defmethod render ((img img-wormtrail-chart))
  (let* ((response (ucw:context.response *context*))
         (stream (ucw::network-stream response)))
      (wormtrails::client-chart stream)))
  

(defmethod imagemap-mouseover ((sample wormtrails::client-sample))
  (format nil "updateBanner(~S, ~S)"
          (format nil "~A (~D)"
                  (name (wormtrails::thing sample))
                  (value sample))
          (wormtrails::html-code (wormtrails::color (wormtrails::thing sample)))))

(defmethod write-html-header ((chart wormtrails::client-chart) stream)
  (html-template:fill-and-print-template #p"client-chart-header.html"  
                                         (list :channel "#lisp")
                                         :stream stream))




