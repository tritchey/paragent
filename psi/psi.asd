;;; -*- Lisp -*-
#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#


(defpackage #:psi-asd
	    (:use :cl :asdf))

(in-package :psi-asd)

(defsystem psi
  :name "psi"
  :version "0.1"
  :components ((:file "defpackage")
	       (:file "queue" :depends-on ("defpackage"))
	       (:file "while" :depends-on ("defpackage"))
	       (:file "guid" :depends-on ("defpackage"))
	       (:file "socket" :depends-on ("defpackage"))
	       (:file "unix-error" :depends-on ("defpackage"))
	       (:file "poll" :depends-on ("unix-error"))
	       #+linux (:file "epoll" :depends-on ("poll"))
	       #+darwin (:file "kqueue" :depends-on ("poll"))
	       (:file "connection" :depends-on ("poll" "queue"))
	       (:file "client" :depends-on ("connection"))
	       (:file "server" :depends-on ("client"))
	       (:file "openssl" :depends-on ("defpackage"))
	       (:file "ssl-connection" :depends-on ("openssl" "client"))
	       (:file "ssl-server" :depends-on ("ssl-connection" "server"))
	       (:file "run-loop" :depends-on ("server" "while")))
  :depends-on (:asdf-binary-locations :sb-bsd-sockets :sb-posix))
