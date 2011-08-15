(in-package :psi)

#+linux (load-shared-object "libssl3.so")
#+darwin (load-shared-object "libssl.dylib")

(defconstant +ssl-filetype-pem+ 1)
(defconstant +ssl-filetype-asn1+ 2)
(defconstant +ssl-filetype-default+ 3)

(defconstant +ssl-error-io+ -10)

(defconstant +ssl_error_none+ 0)
(defconstant +ssl-error-ssl+ 1)
(defconstant +ssl-error-want-read+ 2)
(defconstant +ssl-error-want-write+ 3)
(defconstant +ssl-error-want-x509-lookup+ 4)
(defconstant +ssl-error-syscall+ 5)
(defconstant +ssl-error-zero-return+ 6)
(defconstant +ssl-error-want-connect+ 7)
(defconstant +ssl-error-want-accept+ 8)
(defconstant +ssl-verify-none+ #x00)
(defconstant +ssl-verify-peer+ #x01)
(defconstant +ssl-verify-fail-if-no-peer-cert+ #x02)
(defconstant +ssl-verify-client-once+ #x04)

(defconstant +SSL_CTRL_SET_SESS_CACHE_MODE+ 44)
(defconstant +buffer-size+ 4096)

(defvar *ssl-global-context* nil)
(defvar *ssl-global-method* nil)

;;; NOTE: these are opaque pointers to very hairy ssl structs
;;; that would be a real PITA to fully define. nyef suggested the
;;; use of integer to supress compiler warnings. This may break
;;; on platforms if sizeof(int) != sizeof(void*)
 
(define-alien-type ssl-method integer)
(define-alien-type ssl-ctx integer)
(define-alien-type ssl integer)
(define-alien-type bio integer)

(define-alien-routine ("SSL_load_error_strings" ssl-load-error-strings) void)

(define-alien-routine ("SSL_library_init" ssl-library-init) int)

(define-alien-routine ("SSLv23_method" ssl-v23-method) ssl-method)

(define-alien-routine ("SSL_CTX_new" ssl-ctx-new) ssl-ctx
  (method ssl-method))

(define-alien-routine ("SSL_new" ssl-new) ssl
  (ctx ssl-ctx))

(define-alien-routine ("SSL_free" ssl-free) void
  (ssl ssl))

(define-alien-routine ("BIO_free" bio-free) void
  (bio bio))

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











