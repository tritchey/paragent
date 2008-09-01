#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

;;; this handles the connection from the vnc probe

(in-package :dark-archon)

(defvar *dark-templars* (make-hash-table :test #'equal))
(defvar *observers* (make-hash-table :test #'equal))
(defconstant +buffer-size+ 8192)

(defun make-byte-message (byte)
    (make-array 1 :element-type '(unsigned-byte 8) :initial-element byte))

(defun get-u8 (array index)
  (aref array index))

(defun get-u16 (array index &optional (big-endian t))
  (if big-endian
      (+ (ash (aref array index) 8) (aref array (1+ index)))
      (+ (aref array index) (ash (aref array (1+ index)) 8))))

(defun get-u32 (array index &optional (big-endian t))
  (if big-endian
      (+ (ash (aref array index) 24)
	 (ash (aref array (+ index 1)) 16) 
	 (ash (aref array (+ index 2)) 8) 
	 (aref array (+ index 3)))
      (+ (aref array index) 
	 (ash (aref array (+ index 1)) 8)
	 (ash (aref array (+ index 2)) 16)
	 (ash (aref array (+ index 3)) 24))))

(defun put-u8 (val array index)
  (setf (aref array index) val))

(defun put-u16 (val array index &optional (big-endian t))
  (if big-endian
      (progn
	(setf (aref array index) (ldb (byte 8 8) val))
	(setf (aref array (+ index 1)) (ldb (byte 8 0) val)))
      (progn
	(setf (aref array index) (ldb (byte 8 0) val))
	(setf (aref array (+ index 1)) (ldb (byte 8 8) val)))))

(defun put-u32 (val array index &optional (big-endian t))
  (if big-endian
      (progn
	(setf (aref array index) (ldb (byte 8 24) val))
	(setf (aref array (+ index 1)) (ldb (byte 8 16) val))
	(setf (aref array (+ index 2)) (ldb (byte 8 8) val))
	(setf (aref array (+ index 3)) (ldb (byte 8 0) val)))
      (progn
	(setf (aref array index) (ldb (byte 8 0) val))
	(setf (aref array (+ index 1)) (ldb (byte 8 8) val))
	(setf (aref array (+ index 2)) (ldb (byte 8 16) val))
	(setf (aref array (+ index 3)) (ldb (byte 8 24) val)))))

(defgeneric protocol-version (connection message))
(defgeneric security-type (connection message))
(defgeneric security-response (connection message))
(defgeneric server-init (connection message))
(defgeneric server-handler (connection message))
(defgeneric framebuffer-update-request (connection x-pos y-pos width height &key incremental))
(defgeneric framebuffer-update (connection message))
(defgeneric bytes-per-pixel (connection))

(defclass probe-connection (connection)
  ((version :accessor version
	     :initform nil)
   (security :accessor security
	     :initform nil)
   (width :accessor width
	  :initform 0)
   (height :accessor height
	   :initform 0)
   (bits-per-pixel :accessor bits-per-pixel
		   :initform 0)
   (depth :accessor
	  :initform )
   (big-endian-flag :accessor big-endian-flag
		    :initform nil)
   (true-color-flag :accessor true-color-flag
		    :initform nil)
   (red-max :accessor red-max
	    :initform 0)
   (green-max :accessor green-max
	      :initform 0)
   (blue-max :accessor blue-max
	     :initform 0)
   (red-shift :accessor red-shift
	      :initform 0)
   (green-shift :accessor green-shift
		:initform 0)
   (blue-shift :accessor blue-shift
	       :initform 0)
   (name :accessor name
	 :initform "")
   (message-handler :accessor message-handler
		    :initform #'protocol-version)))

(defclass framebuffer-rect ()
  ((x-pos :accessor x-pos
	  :initarg :x-pos
	  :initform 0)
   (y-pos :accessor y-pos
	  :initarg :y-pos
	  :initform 0)
   (width :accessor width
	  :initarg :width
	  :initform 0)
   (height :accessor height
	   :initarg :height
	   :initform 0)
   (encoding :accessor encoding
	     :initarg :encoding
	     :initform 0)
   (data-length :accessor data-length
		:initarg :data-length
		:initform 0)
   (data-index :accessor data-index
		  :initarg :data-index
		  :initform 0)
   (png-index :accessor png-index
	      :initarg :png-index
	      :initform 0)
   (png :accessor png
	:initarg :png
	:initform nil)))

(defmethod protocol-version ((connection probe-connection) message)
  (record "Determining Protocol")
  (let ((version (subseq (octets-to-string message :external-format :iso-8859-1) 0 11)))
    (record "Protocol Version: ~A" version)
    (when (string= version "RFB 003.008")
      (record "Protocol Version Match")
      (setf (version connection) version)
      (setf (message-handler connection) #'security-type)
      (write-message connection message))))

(defmethod security-type ((connection probe-connection) message)
  (record "Determining Security Types Available")
  (let ((num (aref message 0)))
    (record "Security Types Available: ~A" num)
    (do ((i 1 (+ i 1)))
	((> i num))
      (cond 
	((= (aref message i) 0)
	 (record "Invalid Security Type")
	 (disconnect-event connection))
	((= (aref message i) 1)
	 (record "Security Type: None")
	 ;; this is what we would prefer
	 (setf (security connection) 1)
	 (setf (message-handler connection) #'security-response)
	 (write-message connection (make-byte-message 1)))))))

(defmethod security-response ((connection probe-connection) message)
  (let ((response (get-u32 message 0)))
    (cond 
      ((= response 0)
       (record "Security Response: OK")
	 (setf (message-handler connection) #'server-init)
       (write-message connection (make-byte-message 1)))
      ((= response 1)
       (record "Security Response: Failed")
       (disconnect-event connection))
      (t
       (record "Security Response: Unknown")
       (disconnect-event connection)))))

(defmethod server-init ((connection probe-connection) message)
  (let* ((width (get-u16 message 0))
	 (height (get-u16 message 2))
	 (bits-per-pixel (get-u8 message 4))
	 (depth (get-u8 message 5))
	 (big-endian-flag (get-u8 message 6))
	 (true-color-flag (get-u8 message 7))
	 (red-max (get-u16 message 8))
	 (green-max (get-u16 message 10))
	 (blue-max (get-u16 message 12))
	 (red-shift (get-u8 message 14))
	 (green-shift (get-u8 message 15))
	 (blue-shift (get-u8 message 16))
	 (name-length (get-u32 message 20))
	 (name (octets-to-string (subseq message 24 (+ 24 name-length)) :external-format :iso-8859-1)))
    (record "name: ~a~%width: ~a height: ~a~%bpp: ~a depth: ~a~%big-endian: ~a~%true-color: ~a~%rgb-max: ~a ~a ~a~%rgb-shift: ~a ~a ~a"
	    name 
	    width height 
	    bits-per-pixel depth 
	    big-endian-flag 
	    true-color-flag 
	    red-max blue-max green-max 
	    red-shift green-shift blue-shift)
    (setf (width connection) width)
    (setf (height connection) height)
    (setf (bits-per-pixel connection) bits-per-pixel)
    (setf (big-endian-flag connection) big-endian-flag)
    (setf (true-color-flag connection) true-color-flag)
    (setf (red-max connection) red-max)
    (setf (green-max connection) green-max)
    (setf (blue-max connection) blue-max)
    (setf (red-shift connection) red-shift)
    (setf (green-shift connection) green-shift)
    (setf (blue-shift connection) blue-shift)
    (setf (name connection) name)
    (setf (message-handler connection) #'server-handler)
    (framebuffer-update-request connection 0 0 width height)))

(defmethod server-handler ((connection probe-connection) message)
  (let ((message-type (get-u8 message 0)))
    (case message-type
      (0 (framebuffer-update connection message)))))

(defmethod framebuffer-update-request ((connection probe-connection) x-pos y-pos width height &key (incremental t))
  (let ((message (make-array 10 :element-type '(unsigned-byte 8) :initial-element 0)))
    (put-u8 3 message 0)
    (put-u8 (if incremental 1 0) message 1)
    (put-u16 x-pos message 2)
    (put-u16 y-pos message 4)
    (put-u16 width message 6)
    (put-u16 height message 8)
    (write-message connection message)))

(defmethod framebuffer-update ((connection probe-connection) message)
  (let ((num-rects (get-u16 message 2)))
    (process-rect connection message num-rects 1 4)))

(defmethod process-rect ((connection probe-connection) message num-rects current-rect current-index)
  ;; this is a VERY bad assumption, here, but we are going to assume that the 
  ;; rect preamble doesn't fall across a message boundry
  (let* ((x-pos (get-u16 message (+ current-index 0)))
	 (y-pos (get-u16 message (+ current-index 2)))
	 (width (get-u16 message (+ current-index 4)))
	 (height (get-u16 message (+ current-index 6)))
	 (encoding (get-u32 message (+ current-index 8)))
	 (rect (make-instance 'framebuffer-rect :x-pos x-pos 
						:y-pos y-pos
						:width width
						:height height
						:encoding encoding)))
    (setf (png rect) (make-instance 'zpng:png :width width :height height))
    (case encoding
      (0   (setf (data-length rect) (* (width rect) (height rect) (bytes-per-pixel connection)))
	   (process-raw-rect connection message rect (+ current-index 10))))))

(defmethod process-raw-rect ((connection probe-connection) message rect current-index)
  (record "processing raw rect")
  ;; chunk out whatever we have left in message
  (let ((end (length message))
	(step (bytes-per-pixel connection))
	(image (data-array (png rect)))
	(width (width rect))
	(pixels 0))
    (do  ((i current-index (+ i step))
	  (j (png-index rect) (+ j 1)))
	 ((> i (- end 1)))
      (setf (aref image (floor (/ j width)) (mod j width) 0) 
	    (aref message i)) ;; red pixel component
      (setf (aref image (floor (/ j width)) (mod j width) 1) 
	    (aref message (+ i 1))) ;; green pixel component
      (setf (aref image (floor (/ j width)) (mod j width) 2) 
	    (aref message (+ i 2))) ;; blue pixel component
      (incf pixels))
    (setf (png-index rect) (+ (png-index rect) pixels))
    (setf (data-index rect) (+ (data-index rect) (* pixels 4)))
    (if (= (data-length rect) (data-index rect))
	(progn
	  (record "writing file")
	  (zpng:write-png (png rect) "test.png")
	  (disconnect-event connection)
	  (setf (message-handler connection) #'empty))
	(progn
	  (record "writing file")
	  (zpng:write-png (png rect) "test.png")
	  (disconnect-event connection)
	  (setf (message-handler connection) #'empty)))))
;		(lambda (conn mess)
;		  (process-raw-rect conn mess rect (data-index rect))))))))

(defun empty (foo bar))

(defun test-process-rect (message rect current-index)
  ;; chunk out whatever we have left in message
  (let ((end (length message))
	(step 4)
	(image (data-array (png rect)))
	(width (width rect))
	(pixels 0))
    (do  ((i current-index (+ i step))
	  (j (png-index rect) (+ j 1)))
	 ((> i (- end 1)))
      (setf (aref image (mod j width) (floor (/ j width)) 0) 
	    (aref message i)) ;; red pixel component
      (setf (aref image (mod j width) (floor (/ j width)) 1) 
	    (aref message (+ i 1))) ;; green pixel component
      (setf (aref image (mod j width) (floor (/ j width)) 2) 
	    (aref message (+ i 2))) ;; blue pixel component
      (incf pixels))
    (setf (png-index rect) (+ (png-index rect) pixels))
    (setf (data-index rect) (+ (data-index rect) (* pixels 4)))
    (if (= (data-length rect) (data-index rect))
	(progn
	  (zpng:write-png (png rect) "test.png"))
	(progn
	  (zpng:write-png (png rect) "test.png")))))

    
(defmethod bytes-per-pixel ((connection probe-connection))
  (/ (bits-per-pixel connection) 8))

(defmethod write-message ((connection probe-connection) message)
  (handler-case
    (progn
      (with-mutex ((message-lock connection))
	(enqueue message (outgoing-messages connection)))
      (modify-write-flag connection t))
    (unix-error:ebadf (e)
      (record "WRITE-MESSAGE (PSI) EBADF for ~A: ~A" message e)
      (disconnect-event connection))
    (t (e)
      (record "WRITE-MESSAGE (PSI) error for ~a: ~a" message e))))

(defmethod read-message ((connection probe-connection) message)
  (handler-case
      (funcall (message-handler connection) connection message)
    (end-of-file (e)
      (record "READ-MESSAGE: end-of-file ~a on message ~A~%" e message))
    (undefined-function (e)
      (record "READ-MESSAGE: undefined-function ~a in ~A~%" e message))
    (type-error (e)
      (record "READ-MESSAGE: type-error ~a on message ~A~%" e message))
    (t (e)
      (record "READ-MESSAGE: unknown error: ~a for message ~a" e message))))

(defmethod read-event ((connection probe-connection))
  (multiple-value-bind (buffer num-read) 
      (sb-bsd-sockets:socket-receive (socket connection) nil +buffer-size+ :element-type '(unsigned-byte 8))
    (cond 
      ((and num-read (plusp num-read))
       (read-message connection (subseq buffer 0 num-read)))
      ((and num-read (zerop num-read))
       (disconnect-event connection))
      ((not num-read)
       (disconnect-event connection)))))
