#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#


(in-package :common-lisp-user)

(defpackage :com.paragent.psi
  (:nicknames :psi)
  (:use :cl 
	:sb-alien
	:sb-thread 
	:sb-bsd-sockets
	:sb-ext)
  (:export 
   ;; basic symbols
   :while
   :with-socket
   :make-queue
   :enqueue
   :dequeue
   :make-guid
   
   ;; epoll symbols
   :epoll-create 
   :epoll-control 
   :epoll-wait
   :open-controller 
   :close-controller 
   :control
   :add-watch 
   :delete-watch 
   :modify-watch
   :flags 
   :data1 
   :data2
   :file-descriptor
   :epollable
   :pending-events
   :set-rlimit-nofile 
   :get-rlimit-nofile

   ;; connection symbols
   :socket
   :address
   :epoll-id
   :incoming-message
   :outgoing-message
   :message-lock
   :outgoing-messages
   :event-handler
   :connectedp
   :disconnect-event
   :read-message
   :read-event
   :modify-write-flag
   :write-message
   :write-event
   :handle-connection-event
   :add-connection
   :connection
   :server-connection
   :client-connection
   :ssl-connection
   :ssl-server-connection
   :client-connect

   ;; ssl symbols
   :ssl-initialize

   ;; run-loop
   :start-psi-run-loop
   :stop-psi-run-loop
   :close-connections
   ))
