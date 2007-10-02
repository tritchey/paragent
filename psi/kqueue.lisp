#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#


(in-package :psi)

(defconstant +evfilt-read+ -1)
(defconstant +evfilt-write+ -2)
(defconstant +evfilt_aio+ -3)
(defconstant +evfilt_vnode+ -4)
(defconstant +evfilt_proc+ -5)
(defconstant +evfilt_signal+ -6)
(defconstant +evfilt_syscount+ 6)

;; actions 
(defconstant +ev-add+ #x0001) ;; add event to kq (implies enable)
(defconstant +ev-delete+ #x0002) ;; delete event from kq 
(defconstant +ev-enable+ #x0004) ;; enable event 
(defconstant +ev-disable+ #x0008) ;; disable event (not reported) 
 
;; flags 
(defconstant +ev-oneshot_ #x0010) ;; only report one occurrence 
(defconstant +ev-clear+ #x0020) ;; clear event state after reporting 

(defconstant +ev-sysflags+ #xf000) ;; reserved by system 
(defconstant +ev-flag1+ #x2000) ;; filter-specific flag 
 
;; returned values 
(defconstant +ev-eof+ #x8000) ;; eof detected 
(defconstant +ev-error+ #x4000) ;; error, data contains errno 

;; data/hint flags for evfilt_{read|write}, shared with userspace
(defconstant +note-lowat+ #x0001) ;; low water mark 
(defconstant +note-eof+ #x0002) ;; return on eof 

;; data/hint flags for evfilt_vnode, shared with userspace
(defconstant +note-delete+ #x0001) ;; vnode was removed 
(defconstant +note-write+ #x0002) ;; data contents changed 
(defconstant +note-extend+ #x0004) ;; size increased 
(defconstant +note-attrib+ #x0008) ;; attributes changed 
(defconstant +note-link+ #x0010) ;; link count changed 
(defconstant +note-rename+ #x0020) ;; vnode was renamed 
(defconstant +note-revoke+ #x0040) ;; vnode access was revoked 
(defconstant +note-truncate+ #x0080) ;; vnode was truncated 

;; data/hint flags for evfilt_proc, shared with userspace
(defconstant +note-exit+ #x80000000) ;; process exited 
(defconstant +note-fork+ #x40000000) ;; process forked 
(defconstant +note-exec+ #x20000000) ;; process exec'd 
(defconstant +note-pctrlmask+ #xf0000000) ;; mask for hint bits 
(defconstant +note-pdatamask+ #x000FFFFF) ;; mask for pid 
 
;; additional flags for evfilt_proc 
(defconstant +note-track+ #x00000001) ;; follow across forks 
(defconstant +note-trackerr+ #x00000002) ;; could not track child 
(defconstant +note-child+ #x00000004) ;; am a child process 

(define-alien-type kevent
    (struct kevent 
	    (ident integer)
	    (filter short)
	    (flags unsigned-short)
	    (fflags unsigned-int)
	    (data integer)
	    (udata integer)))

(define-alien-type timespec
    (struct timespec 
	    (tv-sec long)
	    (tv-nsec long)))

(define-alien-routine ("kqueue" kqueue) integer)

(define-alien-routine ("kevent" kevent) integer
  (kq integer)
  (change-list (* (struct kevent)))
  (nchanges integer)
  (event-list (* (struct kevent)))
  (nevents integer)
  (timeout (* (struct timespec))))

(defclass kqueue-event (event)
  ((data :initarg :data
	 :accessor data
	 :initform (make-alien (struct kevent))))
  (:documentation "generic opaque event type that hides the underlying epoll or kevent"))

(defclass kqueue-controller ()
  ((queue :initarg :queue
	  :accessor queue
	  :initform (kqueue))
   (events :initarg :events
	   :accessor events
	   :initform nil))
  (:documentation "class for holding information about a particular queue"))

(defgeneric event-filter (controller fd  flag filter))

(defgeneric add-filter (controller fd filter))

(defgeneric delete-filter (controller fd filter))

(defmethod event-filter ((controller kqueue-controller) fd flag filter)
  (let ((event (make-alien (struct kevent))))
    (setf (slot event 'ident) fd)
    (setf (slot event 'filter) filter)
    (setf (slot event 'flags) flag)
    (kevent (queue controller) event 1 () 0 ())))

(defmethod add-filter ((controller kqueue-controller) fd filter)
  (event-filter controller fd (logior +ev-add+ +ev-enable+) filter))
  
(defmethod delete-filter ((controller kqueue-controller) fd filter)
  (event-filter controller fd +ev-delete+ filter))

(defmethod eofp ((event kqueue-event))
  (plusp (logand (slot (data event) 'flags) +ev-eof+)))

(defmethod readp ((event kqueue-event))
  (equal (slot (data event) 'filter) +evfilt-read+))

(defmethod writep ((event kqueue-event))
  (equal (slot (data event) 'filter) +evfilt-write+))

(defmethod ident ((event kqueue-event))
  (slot (data event) 'ident))

(defmethod add-watch ((controller kqueue-controller) fd &key (read nil default-read-p) 
		      (write nil default-write-p))
  (when default-read-p
    (if read
	(add-filter controller fd +evfilt-read+)
	(delete-filter controller fd +evfilt-read+)))
  (when default-write-p
    (if write
	(add-filter controller fd +evfilt-write+)
	(delete-filter controller fd +evfilt-write+))))


(defmethod modify-watch ((controller kqueue-controller) fd &key (read nil default-read-p) 
			 (write nil default-write-p))
  (when default-read-p
    (if read
	(add-filter controller fd +evfilt-read+)
	(delete-filter controller fd +evfilt-read+)))
  (when default-write-p
    (if write
	(add-filter controller fd +evfilt-write+)
	(delete-filter controller fd +evfilt-write+))))

(defmethod delete-watch ((controller kqueue-controller) fd)
  (delete-filter controller fd +evfilt-read+)
  (delete-filter controller fd +evfilt-write+))


(defmethod pending-events ((controller kqueue-controller) &optional (timeout nil))
  (declare (ignore timeout))
      (let* ((kevents (make-alien (array (struct kevent) 10)))
	     (num-events (kevent (queue controller) () 0 (sb-alien:alien-sap kevents) 10 nil)))
	(cond 
	  ((plusp num-events)
	   (let ((events (make-array num-events)))
	     (loop for i below num-events do
		  (setf (aref events i) 
			(make-instance 'kqueue-event :data (deref (deref kevents 0) i))))
	     events))
	  ((minusp num-events)
	   (unix-error::perror (unix-error:errno))))))

(defmethod create-poll-controller ()
  (make-instance 'kqueue-controller))

