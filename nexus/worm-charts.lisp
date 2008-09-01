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
  (multiple-value-bind
	(second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (declare (ignore second minute hour date month year dst-p tz))
    (let* ((index (index day-bucket))
	   (dow (mod (+ day-of-week index 2) 7)))
      (cond ((= dow 0)
           "SUNDAY")
	  ((= dow 1)
	   "MONDAY")
	  ((= dow 2)
	   "TUESDAY")
	  ((= dow 3)
	   "WEDNESDAY")
	  ((= dow 4)
	   "THURSDAY")
	  ((= dow 5)
	   "FRIDAY")
	  ((= dow 6)
	   "SATURDAY")))))


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

(defun highest-activity (client-worm)
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
    (wormtrails::output-png-stream chart stream
                 :scaler (wormtrails::linear-scaler scale)
                 :metric-height 20 :metric-label "10 EVENTS")))

(in-package :nexus)

#.(clsql:locally-enable-sql-reader-syntax)

(defmacro <wormtrail-chart (page clients)
  `(<:img :src (make-link (show-wormtrail-chart ,page ,clients))))

(defaction show-wormtrail-chart ((page paragent-component) clients)
  (call 'img-wormtrail-chart :clients clients))

(defcomponent img-wormtrail-chart (png-component) 
  ((clients :accessor clients
           :initarg :clients
           :initform nil)))

(defmethod render ((img img-wormtrail-chart))
  (let* ((response (ucw:context.response *context*))
         (stream (ucw::network-stream response))
	 (clients (clients img))
	 (chart (make-instance 'wormtrails::client-chart))
	 (scale 1.0)
	 (wormtrails::*font-file* "/Users/tritchey/Projects/Paragent/git/nexus/font.ttf")
         (duration 6))
    (dolist (client clients)
	(let* ((name (name client)))
	  (loop for i from duration downto 0 do
	       (let ((count
		      (caar
		       (with-db (query (format nil "select count(events.id) from events join computers on events.computer_id = computers.id where DATE(timestamp)=DATE_SUB(DATE(NOW()), INTERVAL ~a DAY) and computers.client_id=~a" i (id client)))))))
		 (wormtrails::add-data chart (- 6 i) name count)))))
    (wormtrails::output-png-stream chart stream
				   :scaler (wormtrails::linear-scaler scale)
				   :metric-height 100 :metric-label "100 EVENTS")))


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

#.(clsql:restore-sql-reader-syntax-state)



