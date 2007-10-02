#|
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

(defpackage :com.paragent.openssl
  (:nicknames :openssl)
  (:use :cl :sb-alien :sb-bsd-sockets))

(in-package :openssl)

(load-shared-object "libssl.so")

(defconstant +ssl-filetype-pem+ 1)
(defconstant +ssl-filetype-asn1+ 2)
(defconstant +ssl-filetype-default+ 3)

(defconstant +ssl-error-none+ 0)
(defconstant +ssl-error-ssl+ 1)
(defconstant +ssl-error-want-read+ 2)
(defconstant +ssl-error-want-write+ 3)
(defconstant +ssl-error-want-x509-lookup+ 4)
(defconstant +ssl-error-syscall+ 5)
(defconstant +ssl-error-zero-return+ 6)
(defconstant +ssl-error-want-connect+ 7)
(defconstant +ssl-error-want-accept+ 8)

(defconstant +ssl_ctrl_set_sess_cache_mode+ 44)
(defconstant +buffer-size+ 4096)

(defvar *ssl-global-context* nil)
(defvar *ssl-global-method* nil)

(define-alien-type ssl-method (* t))
(define-alien-type ssl-ctx (* t))
(define-alien-type ssl (* t))
(define-alien-type bio (* t))

(define-alien-routine ("SSL_load_error_strings" ssl-load-error-strings) void)

(define-alien-routine ("SSL_library_init" ssl-library-init) int)

(define-alien-routine ("SSLv23_method" ssl-v23-method) ssl-method)

(define-alien-routine ("SSL_CTX_new" ssl-ctx-new) ssl-ctx
  (method ssl-method))

(define-alien-routine ("SSL_new" ssl-new) ssl
  (ctx ssl-ctx))

(define-alien-routine ("SSL_free" ssl-free) void
  (ssl ssl))

(define-alien-routine ("SSL_set_accept_state" ssl-set-accept-state) void
  (ssl ssl))

(define-alien-routine ("SSL_CTX_ctrl" ssl-ctx-ctrl) long
  (ctx ssl-ctx)
  (cmd int)
  (larg long)
  (parg long))

(define-alien-routine ("SSL_set_cipher_list" ssl-set-cipher-list) int
  (ssl ssl)
  (str c-string))

(define-alien-routine ("SSL_CTX_set_cipher_list" ssl-ctx-set-cipher-list) int
  (ctx ssl-ctx)
  (str c-string))

(define-alien-routine ("SSL_use_RSAPrivateKey_file" ssl-use-rsa-privatekey-file) int
  (ssl ssl)
  (str c-string)
  (type int))

(define-alien-routine ("SSL_CTX_use_RSAPrivateKey_file" ssl-ctx-use-rsa-privatekey-file) int
  (ctx ssl-ctx)
  (str c-string)
  (type int))

(define-alien-routine ("SSL_use_certificate_file" ssl-use-certificate-file) int
  (ssl ssl)
  (str c-string)
  (type int))

(define-alien-routine ("SSL_CTX_use_certificate_file" ssl-ctx-use-certificate-file) int
  (ctx ssl-ctx)
  (str c-string)
  (type int))

(define-alien-routine ("BIO_new_socket" bio-new-socket) bio
  (sock int)
  (close-flag int))

(define-alien-routine ("SSL_set_bio" ssl-set-bio) void
  (ssl ssl)
  (rbio bio)
  (wbio bio))

(define-alien-routine ("SSL_accept" ssl-accept) int
  (ssl ssl))

(define-alien-routine ("SSL_read" ssl-read) int
  (ssl ssl)
  (buf (* t))
  (num int))

(define-alien-routine ("SSL_write" ssl-write) int
  (ssl ssl)
  (buf (* t))
  (num int))

(define-alien-routine ("SSL_get_error" ssl-get-error) int
  (ssl ssl)
  (ret int))


(define-condition ssl-error (error)
  ((queue :initform nil :initarg :queue :reader ssl-error-queue)))

(define-condition ssl-error-initialize (ssl-error)
  ((reason  :initarg :reason
            :reader ssl-error-reason))
  (:report (lambda (condition stream)
             (format stream "SSL initialization error: ~A"
                     (ssl-error-reason condition))
	     (write-sequence (ssl-error-queue condition) stream))))


(defun ssl-ctx-set-session-cache-mode (ctx mode)
  (ssl-ctx-ctrl ctx +SSL_CTRL_SET_SESS_CACHE_MODE+ mode 0))

(defun make-global-context (&key certificate key)
  (setf *ssl-global-method* (ssl-v23-method))
  (setf *ssl-global-context* (ssl-ctx-new *ssl-global-method*))
  (ssl-ctx-set-session-cache-mode *ssl-global-context* 3)
  (when (zerop (ssl-ctx-set-cipher-list *ssl-global-context* "ALL"))
    (error 'ssl-error-initialize :reason "Can't set SSL cipher list"))
  (when key
    (unless (eql 1 (ssl-ctx-use-rsa-privatekey-file *ssl-global-context*
						    key
						    +ssl-filetype-pem+))
      (error 'ssl-error-initialize :reason "Can't load RSA private key ~A")))
  (when certificate
    (unless (eql 1 (ssl-ctx-use-certificate-file *ssl-global-context*
						 certificate
						 +ssl-filetype-pem+))
      (error 'ssl-error-initialize
	     :reason "Can't load certificate ~A" certificate))))

(defun initialize ()
  (ssl-load-error-strings)
  (ssl-library-init)
  (make-global-context :certificate "/lisp/repos/ca/archoncert.pem"
		       :key "/lisp/repos/ca/archonkey.pem"))










