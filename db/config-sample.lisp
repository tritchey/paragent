#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

(in-package :db)

(defparameter *db-library-path* "/usr/local/lib/mysql/")
(defparameter *db-database* "database")
(defparameter *db-username* "username")
(defparameter *db-password* "password")

(defparameter *email-from-address* "support@yourcompany.com")
(defparameter *email-server* "smtp.yourcompany.com")
(defparameter *email-smtp-authentication* t)
(defparameter *email-account-name* "username")
(defparameter *email-account-password* "password")

(defparameter *credit-card-login* "")
(defparameter *credit-card-password* "")

(defparameter *server-url* "http://test.paragent.com/")

(defparameter *administrator-notification-email* '("someone@yourcompany.com"))

(defparameter *templar-ssl-cert* "/path/to/certs/testcert.pem")
(defparameter *templar-ssl-key* "/path/to/certs/testkey.pem")

(defparameter *default-arbiter-templar-port* 6103
  "the port that arbiter listens for connections from templar agents")
(defparameter *default-arbiter-archon-port* 6105
  "the port that arbiter listens for connections from archon instances")
(defparameter *default-arbiter-nexus-port* 6106
  "the port that arbiter listens for connections from nexus instances")
(defparameter *default-arbiter-address* #(127 0 0 1)
  "the address that your arbiter is running")

(defparameter *default-dark-archon-server* "your.reflector.com"
  "The server name for the remote desktop reflector")

