;;; Mostly auto-generated from unix-errors.lisp.
;;; Not expected to change much any more.
;;; Courtesy of Xach from #lisp (Zach Beane)
;;; $Id: unix-error.lisp,v 1.2 2006/04/27 21:17:38 xach Exp $

(defpackage :unix-error
            (:use :cl)
            (:shadow #:number #:signal)
            (:export #:unix-error-name
                     #:unix-error-number
                     #:unix-error-description
                     #:errno
                     #:signal)
            (:export #:eperm
                     #:enoent
                     #:esrch
                     #:eintr
                     #:eio
                     #:enxio
                     #:e2big
                     #:enoexec
                     #:ebadf
                     #:echild
                     #:eagain
                     #:enomem
                     #:eacces
                     #:efault
                     #:enotblk
                     #:ebusy
                     #:eexist
                     #:exdev
                     #:enodev
                     #:enotdir
                     #:eisdir
                     #:einval
                     #:enfile
                     #:emfile
                     #:enotty
                     #:etxtbsy
                     #:efbig
                     #:enospc
                     #:espipe
                     #:erofs
                     #:emlink
                     #:epipe
                     #:edom
                     #:erange
                     #:edeadlk
                     #:enametoolong
                     #:enolck
                     #:enosys
                     #:enotempty
                     #:eloop
                     #:ewouldblock
                     #:enomsg
                     #:eidrm
                     #:echrng
                     #:el2nsync
                     #:el3hlt
                     #:el3rst
                     #:elnrng
                     #:eunatch
                     #:enocsi
                     #:el2hlt
                     #:ebade
                     #:ebadr
                     #:exfull
                     #:enoano
                     #:ebadrqc
                     #:ebadslt
                     #:ebfont
                     #:enostr
                     #:enodata
                     #:etime
                     #:enosr
                     #:enonet
                     #:enopkg
                     #:eremote
                     #:enolink
                     #:eadv
                     #:esrmnt
                     #:ecomm
                     #:eproto
                     #:emultihop
                     #:edotdot
                     #:ebadmsg
                     #:eoverflow
                     #:enotuniq
                     #:ebadfd
                     #:eremchg
                     #:elibacc
                     #:elibbad
                     #:elibscn
                     #:elibmax
                     #:elibexec
                     #:eilseq
                     #:erestart
                     #:estrpipe
                     #:eusers
                     #:enotsock
                     #:edestaddrreq
                     #:emsgsize
                     #:eprototype
                     #:enoprotoopt
                     #:eprotonosupport
                     #:esocktnosupport
                     #:eopnotsupp
                     #:epfnosupport
                     #:eafnosupport
                     #:eaddrinuse
                     #:eaddrnotavail
                     #:enetdown
                     #:enetunreach
                     #:enetreset
                     #:econnaborted
                     #:econnreset
                     #:enobufs
                     #:eisconn
                     #:enotconn
                     #:eshutdown
                     #:etoomanyrefs
                     #:etimedout
                     #:econnrefused
                     #:ehostdown
                     #:ehostunreach
                     #:ealready
                     #:einprogress
                     #:estale
                     #:euclean
                     #:enotnam
                     #:enavail
                     #:eisnam
                     #:eremoteio
                     #:edquot
                     #:enomedium
                     #:emediumtype))

(in-package :unix-error)
(defconstant eperm 1 "Operation not permitted")
(defconstant enoent 2 "No such file or directory")
(defconstant esrch 3 "No such process")
(defconstant eintr 4 "Interrupted system call")
(defconstant eio 5 "I/O error")
(defconstant enxio 6 "No such device or address")
(defconstant e2big 7 "Arg list too long")
(defconstant enoexec 8 "Exec format error")
(defconstant ebadf 9 "Bad file number")
(defconstant echild 10 "No child processes")
(defconstant eagain 11 "Try again")
(defconstant enomem 12 "Out of memory")
(defconstant eacces 13 "Permission denied")
(defconstant efault 14 "Bad address")
(defconstant enotblk 15 "Block device required")
(defconstant ebusy 16 "Device or resource busy")
(defconstant eexist 17 "File exists")
(defconstant exdev 18 "Cross-device link")
(defconstant enodev 19 "No such device")
(defconstant enotdir 20 "Not a directory")
(defconstant eisdir 21 "Is a directory")
(defconstant einval 22 "Invalid argument")
(defconstant enfile 23 "File table overflow")
(defconstant emfile 24 "Too many open files")
(defconstant enotty 25 "Not a typewriter")
(defconstant etxtbsy 26 "Text file busy")
(defconstant efbig 27 "File too large")
(defconstant enospc 28 "No space left on device")
(defconstant espipe 29 "Illegal seek")
(defconstant erofs 30 "Read-only file system")
(defconstant emlink 31 "Too many links")
(defconstant epipe 32 "Broken pipe")
(defconstant edom 33 "Math argument out of domain of func")
(defconstant erange 34 "Math result not representable")
(defconstant edeadlk 35 "Resource deadlock would occur")
(defconstant enametoolong 36 "File name too long")
(defconstant enolck 37 "No record locks available")
(defconstant enosys 38 "Function not implemented")
(defconstant enotempty 39 "Directory not empty")
(defconstant eloop 40 "Too many symbolic links encountered")
(defconstant ewouldblock 11 "Operation would block")
(defconstant enomsg 42 "No message of desired type")
(defconstant eidrm 43 "Identifier removed")
(defconstant echrng 44 "Channel number out of range")
(defconstant el2nsync 45 "Level 2 not synchronized")
(defconstant el3hlt 46 "Level 3 halted")
(defconstant el3rst 47 "Level 3 reset")
(defconstant elnrng 48 "Link number out of range")
(defconstant eunatch 49 "Protocol driver not attached")
(defconstant enocsi 50 "No CSI structure available")
(defconstant el2hlt 51 "Level 2 halted")
(defconstant ebade 52 "Invalid exchange")
(defconstant ebadr 53 "Invalid request descriptor")
(defconstant exfull 54 "Exchange full")
(defconstant enoano 55 "No anode")
(defconstant ebadrqc 56 "Invalid request code")
(defconstant ebadslt 57 "Invalid slot")
(defconstant ebfont 59 "Bad font file format")
(defconstant enostr 60 "Device not a stream")
(defconstant enodata 61 "No data available")
(defconstant etime 62 "Timer expired")
(defconstant enosr 63 "Out of streams resources")
(defconstant enonet 64 "Machine is not on the network")
(defconstant enopkg 65 "Package not installed")
(defconstant eremote 66 "Object is remote")
(defconstant enolink 67 "Link has been severed")
(defconstant eadv 68 "Advertise error")
(defconstant esrmnt 69 "Srmount error")
(defconstant ecomm 70 "Communication error on send")
(defconstant eproto 71 "Protocol error")
(defconstant emultihop 72 "Multihop attempted")
(defconstant edotdot 73 "RFS specific error")
(defconstant ebadmsg 74 "Not a data message")
(defconstant eoverflow 75 "Value too large for defined data type")
(defconstant enotuniq 76 "Name not unique on network")
(defconstant ebadfd 77 "File descriptor in bad state")
(defconstant eremchg 78 "Remote address changed")
(defconstant elibacc 79 "Can not access a needed shared library")
(defconstant elibbad 80 "Accessing a corrupted shared library")
(defconstant elibscn 81 ".lib section in a.out corrupted")
(defconstant elibmax 82 "Attempting to link in too many shared libraries")
(defconstant elibexec 83 "Cannot exec a shared library directly")
(defconstant eilseq 84 "Illegal byte sequence")
(defconstant erestart 85 "Interrupted system call should be restarted")
(defconstant estrpipe 86 "Streams pipe error")
(defconstant eusers 87 "Too many users")
(defconstant enotsock 88 "Socket operation on non-socket")
(defconstant edestaddrreq 89 "Destination address required")
(defconstant emsgsize 90 "Message too long")
(defconstant eprototype 91 "Protocol wrong type for socket")
(defconstant enoprotoopt 92 "Protocol not available")
(defconstant eprotonosupport 93 "Protocol not supported")
(defconstant esocktnosupport 94 "Socket type not supported")
(defconstant eopnotsupp 95 "Operation not supported on transport endpoint")
(defconstant epfnosupport 96 "Protocol family not supported")
(defconstant eafnosupport 97 "Address family not supported by protocol")
(defconstant eaddrinuse 98 "Address already in use")
(defconstant eaddrnotavail 99 "Cannot assign requested address")
(defconstant enetdown 100 "Network is down")
(defconstant enetunreach 101 "Network is unreachable")
(defconstant enetreset 102 "Network dropped connection because of reset")
(defconstant econnaborted 103 "Software caused connection abort")
(defconstant econnreset 104 "Connection reset by peer")
(defconstant enobufs 105 "No buffer space available")
(defconstant eisconn 106 "Transport endpoint is already connected")
(defconstant enotconn 107 "Transport endpoint is not connected")
(defconstant eshutdown 108 "Cannot send after transport endpoint shutdown")
(defconstant etoomanyrefs 109 "Too many references: cannot splice")
(defconstant etimedout 110 "Connection timed out")
(defconstant econnrefused 111 "Connection refused")
(defconstant ehostdown 112 "Host is down")
(defconstant ehostunreach 113 "No route to host")
(defconstant ealready 114 "Operation already in progress")
(defconstant einprogress 115 "Operation now in progress")
(defconstant estale 116 "Stale NFS file handle")
(defconstant euclean 117 "Structure needs cleaning")
(defconstant enotnam 118 "Not a XENIX named type file")
(defconstant enavail 119 "No XENIX semaphores available")
(defconstant eisnam 120 "Is a named type file")
(defconstant eremoteio 121 "Remote I/O error")
(defconstant edquot 122 "Quota exceeded")
(defconstant enomedium 123 "No medium found")
(defconstant emediumtype 124 "Wrong medium type")


(define-condition unix-error (error)
  ((name :initarg :name :accessor unix-error-name)
   (number :initarg :number :accessor unix-error-number)
   (description  :initarg  :description
    :accessor unix-error-description)
   (from :initarg :from :initform nil :accessor unix-error-from))
  (:report (lambda (c s)
             (format s "~:[~;~:*~A: ~]Unix error ~D (~A): ~A"
                     (unix-error-from c)
                     (unix-error-number c)
                     (unix-error-name c)
                     (unix-error-description c)))))

(define-condition eperm
                  (unix-error)
                  ((name :initform 'eperm) (number :initform 1)
                   (description :initform "Operation not permitted")))

(define-condition enoent
                  (unix-error)
                  ((name :initform 'enoent) (number :initform 2)
                   (description :initform "No such file or directory")))

(define-condition esrch
                  (unix-error)
                  ((name :initform 'esrch) (number :initform 3)
                   (description :initform "No such process")))

(define-condition eintr
                  (unix-error)
                  ((name :initform 'eintr) (number :initform 4)
                   (description :initform "Interrupted system call")))

(define-condition eio
                  (unix-error)
                  ((name :initform 'eio) (number :initform 5)
                   (description :initform "I/O error")))

(define-condition enxio
                  (unix-error)
                  ((name :initform 'enxio) (number :initform 6)
                   (description :initform "No such device or address")))

(define-condition e2big
                  (unix-error)
                  ((name :initform 'e2big) (number :initform 7)
                   (description :initform "Arg list too long")))

(define-condition enoexec
                  (unix-error)
                  ((name :initform 'enoexec) (number :initform 8)
                   (description :initform "Exec format error")))

(define-condition ebadf
                  (unix-error)
                  ((name :initform 'ebadf) (number :initform 9)
                   (description :initform "Bad file number")))

(define-condition echild
                  (unix-error)
                  ((name :initform 'echild) (number :initform 10)
                   (description :initform "No child processes")))

(define-condition eagain
                  (unix-error)
                  ((name :initform 'eagain) (number :initform 11)
                   (description :initform "Try again")))

(define-condition enomem
                  (unix-error)
                  ((name :initform 'enomem) (number :initform 12)
                   (description :initform "Out of memory")))

(define-condition eacces
                  (unix-error)
                  ((name :initform 'eacces) (number :initform 13)
                   (description :initform "Permission denied")))

(define-condition efault
                  (unix-error)
                  ((name :initform 'efault) (number :initform 14)
                   (description :initform "Bad address")))

(define-condition enotblk
                  (unix-error)
                  ((name :initform 'enotblk) (number :initform 15)
                   (description :initform "Block device required")))

(define-condition ebusy
                  (unix-error)
                  ((name :initform 'ebusy) (number :initform 16)
                   (description :initform "Device or resource busy")))

(define-condition eexist
                  (unix-error)
                  ((name :initform 'eexist) (number :initform 17)
                   (description :initform "File exists")))

(define-condition exdev
                  (unix-error)
                  ((name :initform 'exdev) (number :initform 18)
                   (description :initform "Cross-device link")))

(define-condition enodev
                  (unix-error)
                  ((name :initform 'enodev) (number :initform 19)
                   (description :initform "No such device")))

(define-condition enotdir
                  (unix-error)
                  ((name :initform 'enotdir) (number :initform 20)
                   (description :initform "Not a directory")))

(define-condition eisdir
                  (unix-error)
                  ((name :initform 'eisdir) (number :initform 21)
                   (description :initform "Is a directory")))

(define-condition einval
                  (unix-error)
                  ((name :initform 'einval) (number :initform 22)
                   (description :initform "Invalid argument")))

(define-condition enfile
                  (unix-error)
                  ((name :initform 'enfile) (number :initform 23)
                   (description :initform "File table overflow")))

(define-condition emfile
                  (unix-error)
                  ((name :initform 'emfile) (number :initform 24)
                   (description :initform "Too many open files")))

(define-condition enotty
                  (unix-error)
                  ((name :initform 'enotty) (number :initform 25)
                   (description :initform "Not a typewriter")))

(define-condition etxtbsy
                  (unix-error)
                  ((name :initform 'etxtbsy) (number :initform 26)
                   (description :initform "Text file busy")))

(define-condition efbig
                  (unix-error)
                  ((name :initform 'efbig) (number :initform 27)
                   (description :initform "File too large")))

(define-condition enospc
                  (unix-error)
                  ((name :initform 'enospc) (number :initform 28)
                   (description :initform "No space left on device")))

(define-condition espipe
                  (unix-error)
                  ((name :initform 'espipe) (number :initform 29)
                   (description :initform "Illegal seek")))

(define-condition erofs
                  (unix-error)
                  ((name :initform 'erofs) (number :initform 30)
                   (description :initform "Read-only file system")))

(define-condition emlink
                  (unix-error)
                  ((name :initform 'emlink) (number :initform 31)
                   (description :initform "Too many links")))

(define-condition epipe
                  (unix-error)
                  ((name :initform 'epipe) (number :initform 32)
                   (description :initform "Broken pipe")))

(define-condition edom
                  (unix-error)
                  ((name :initform 'edom) (number :initform 33)
                   (description :initform
                                "Math argument out of domain of func")))

(define-condition erange
                  (unix-error)
                  ((name :initform 'erange) (number :initform 34)
                   (description :initform "Math result not representable")))

(define-condition edeadlk
                  (unix-error)
                  ((name :initform 'edeadlk) (number :initform 35)
                   (description :initform "Resource deadlock would occur")))

(define-condition enametoolong
                  (unix-error)
                  ((name :initform 'enametoolong) (number :initform 36)
                   (description :initform "File name too long")))

(define-condition enolck
                  (unix-error)
                  ((name :initform 'enolck) (number :initform 37)
                   (description :initform "No record locks available")))

(define-condition enosys
                  (unix-error)
                  ((name :initform 'enosys) (number :initform 38)
                   (description :initform "Function not implemented")))

(define-condition enotempty
                  (unix-error)
                  ((name :initform 'enotempty) (number :initform 39)
                   (description :initform "Directory not empty")))

(define-condition eloop
                  (unix-error)
                  ((name :initform 'eloop) (number :initform 40)
                   (description :initform
                                "Too many symbolic links encountered")))

(define-condition ewouldblock
                  (unix-error)
                  ((name :initform 'ewouldblock) (number :initform 11)
                   (description :initform "Operation would block")))

(define-condition enomsg
                  (unix-error)
                  ((name :initform 'enomsg) (number :initform 42)
                   (description :initform "No message of desired type")))

(define-condition eidrm
                  (unix-error)
                  ((name :initform 'eidrm) (number :initform 43)
                   (description :initform "Identifier removed")))

(define-condition echrng
                  (unix-error)
                  ((name :initform 'echrng) (number :initform 44)
                   (description :initform "Channel number out of range")))

(define-condition el2nsync
                  (unix-error)
                  ((name :initform 'el2nsync) (number :initform 45)
                   (description :initform "Level 2 not synchronized")))

(define-condition el3hlt
                  (unix-error)
                  ((name :initform 'el3hlt) (number :initform 46)
                   (description :initform "Level 3 halted")))

(define-condition el3rst
                  (unix-error)
                  ((name :initform 'el3rst) (number :initform 47)
                   (description :initform "Level 3 reset")))

(define-condition elnrng
                  (unix-error)
                  ((name :initform 'elnrng) (number :initform 48)
                   (description :initform "Link number out of range")))

(define-condition eunatch
                  (unix-error)
                  ((name :initform 'eunatch) (number :initform 49)
                   (description :initform "Protocol driver not attached")))

(define-condition enocsi
                  (unix-error)
                  ((name :initform 'enocsi) (number :initform 50)
                   (description :initform "No CSI structure available")))

(define-condition el2hlt
                  (unix-error)
                  ((name :initform 'el2hlt) (number :initform 51)
                   (description :initform "Level 2 halted")))

(define-condition ebade
                  (unix-error)
                  ((name :initform 'ebade) (number :initform 52)
                   (description :initform "Invalid exchange")))

(define-condition ebadr
                  (unix-error)
                  ((name :initform 'ebadr) (number :initform 53)
                   (description :initform "Invalid request descriptor")))

(define-condition exfull
                  (unix-error)
                  ((name :initform 'exfull) (number :initform 54)
                   (description :initform "Exchange full")))

(define-condition enoano
                  (unix-error)
                  ((name :initform 'enoano) (number :initform 55)
                   (description :initform "No anode")))

(define-condition ebadrqc
                  (unix-error)
                  ((name :initform 'ebadrqc) (number :initform 56)
                   (description :initform "Invalid request code")))

(define-condition ebadslt
                  (unix-error)
                  ((name :initform 'ebadslt) (number :initform 57)
                   (description :initform "Invalid slot")))

(define-condition ebfont
                  (unix-error)
                  ((name :initform 'ebfont) (number :initform 59)
                   (description :initform "Bad font file format")))

(define-condition enostr
                  (unix-error)
                  ((name :initform 'enostr) (number :initform 60)
                   (description :initform "Device not a stream")))

(define-condition enodata
                  (unix-error)
                  ((name :initform 'enodata) (number :initform 61)
                   (description :initform "No data available")))

(define-condition etime
                  (unix-error)
                  ((name :initform 'etime) (number :initform 62)
                   (description :initform "Timer expired")))

(define-condition enosr
                  (unix-error)
                  ((name :initform 'enosr) (number :initform 63)
                   (description :initform "Out of streams resources")))

(define-condition enonet
                  (unix-error)
                  ((name :initform 'enonet) (number :initform 64)
                   (description :initform "Machine is not on the network")))

(define-condition enopkg
                  (unix-error)
                  ((name :initform 'enopkg) (number :initform 65)
                   (description :initform "Package not installed")))

(define-condition eremote
                  (unix-error)
                  ((name :initform 'eremote) (number :initform 66)
                   (description :initform "Object is remote")))

(define-condition enolink
                  (unix-error)
                  ((name :initform 'enolink) (number :initform 67)
                   (description :initform "Link has been severed")))

(define-condition eadv
                  (unix-error)
                  ((name :initform 'eadv) (number :initform 68)
                   (description :initform "Advertise error")))

(define-condition esrmnt
                  (unix-error)
                  ((name :initform 'esrmnt) (number :initform 69)
                   (description :initform "Srmount error")))

(define-condition ecomm
                  (unix-error)
                  ((name :initform 'ecomm) (number :initform 70)
                   (description :initform "Communication error on send")))

(define-condition eproto
                  (unix-error)
                  ((name :initform 'eproto) (number :initform 71)
                   (description :initform "Protocol error")))

(define-condition emultihop
                  (unix-error)
                  ((name :initform 'emultihop) (number :initform 72)
                   (description :initform "Multihop attempted")))

(define-condition edotdot
                  (unix-error)
                  ((name :initform 'edotdot) (number :initform 73)
                   (description :initform "RFS specific error")))

(define-condition ebadmsg
                  (unix-error)
                  ((name :initform 'ebadmsg) (number :initform 74)
                   (description :initform "Not a data message")))

(define-condition eoverflow
                  (unix-error)
                  ((name :initform 'eoverflow) (number :initform 75)
                   (description :initform
                                "Value too large for defined data type")))

(define-condition enotuniq
                  (unix-error)
                  ((name :initform 'enotuniq) (number :initform 76)
                   (description :initform "Name not unique on network")))

(define-condition ebadfd
                  (unix-error)
                  ((name :initform 'ebadfd) (number :initform 77)
                   (description :initform "File descriptor in bad state")))

(define-condition eremchg
                  (unix-error)
                  ((name :initform 'eremchg) (number :initform 78)
                   (description :initform "Remote address changed")))

(define-condition elibacc
                  (unix-error)
                  ((name :initform 'elibacc) (number :initform 79)
                   (description :initform
                                "Can not access a needed shared library")))

(define-condition elibbad
                  (unix-error)
                  ((name :initform 'elibbad) (number :initform 80)
                   (description :initform
                                "Accessing a corrupted shared library")))

(define-condition elibscn
                  (unix-error)
                  ((name :initform 'elibscn) (number :initform 81)
                   (description :initform ".lib section in a.out corrupted")))

(define-condition elibmax
                  (unix-error)
                  ((name :initform 'elibmax) (number :initform 82)
                   (description :initform
                                "Attempting to link in too many shared libraries")))

(define-condition elibexec
                  (unix-error)
                  ((name :initform 'elibexec) (number :initform 83)
                   (description :initform
                                "Cannot exec a shared library directly")))

(define-condition eilseq
                  (unix-error)
                  ((name :initform 'eilseq) (number :initform 84)
                   (description :initform "Illegal byte sequence")))

(define-condition erestart
                  (unix-error)
                  ((name :initform 'erestart) (number :initform 85)
                   (description :initform
                                "Interrupted system call should be restarted")))

(define-condition estrpipe
                  (unix-error)
                  ((name :initform 'estrpipe) (number :initform 86)
                   (description :initform "Streams pipe error")))

(define-condition eusers
                  (unix-error)
                  ((name :initform 'eusers) (number :initform 87)
                   (description :initform "Too many users")))

(define-condition enotsock
                  (unix-error)
                  ((name :initform 'enotsock) (number :initform 88)
                   (description :initform "Socket operation on non-socket")))

(define-condition edestaddrreq
                  (unix-error)
                  ((name :initform 'edestaddrreq) (number :initform 89)
                   (description :initform "Destination address required")))

(define-condition emsgsize
                  (unix-error)
                  ((name :initform 'emsgsize) (number :initform 90)
                   (description :initform "Message too long")))

(define-condition eprototype
                  (unix-error)
                  ((name :initform 'eprototype) (number :initform 91)
                   (description :initform "Protocol wrong type for socket")))

(define-condition enoprotoopt
                  (unix-error)
                  ((name :initform 'enoprotoopt) (number :initform 92)
                   (description :initform "Protocol not available")))

(define-condition eprotonosupport
                  (unix-error)
                  ((name :initform 'eprotonosupport) (number :initform 93)
                   (description :initform "Protocol not supported")))

(define-condition esocktnosupport
                  (unix-error)
                  ((name :initform 'esocktnosupport) (number :initform 94)
                   (description :initform "Socket type not supported")))

(define-condition eopnotsupp
                  (unix-error)
                  ((name :initform 'eopnotsupp) (number :initform 95)
                   (description :initform
                                "Operation not supported on transport endpoint")))

(define-condition epfnosupport
                  (unix-error)
                  ((name :initform 'epfnosupport) (number :initform 96)
                   (description :initform "Protocol family not supported")))

(define-condition eafnosupport
                  (unix-error)
                  ((name :initform 'eafnosupport) (number :initform 97)
                   (description :initform
                                "Address family not supported by protocol")))

(define-condition eaddrinuse
                  (unix-error)
                  ((name :initform 'eaddrinuse) (number :initform 98)
                   (description :initform "Address already in use")))

(define-condition eaddrnotavail
                  (unix-error)
                  ((name :initform 'eaddrnotavail) (number :initform 99)
                   (description :initform "Cannot assign requested address")))

(define-condition enetdown
                  (unix-error)
                  ((name :initform 'enetdown) (number :initform 100)
                   (description :initform "Network is down")))

(define-condition enetunreach
                  (unix-error)
                  ((name :initform 'enetunreach) (number :initform 101)
                   (description :initform "Network is unreachable")))

(define-condition enetreset
                  (unix-error)
                  ((name :initform 'enetreset) (number :initform 102)
                   (description :initform
                                "Network dropped connection because of reset")))

(define-condition econnaborted
                  (unix-error)
                  ((name :initform 'econnaborted) (number :initform 103)
                   (description :initform "Software caused connection abort")))

(define-condition econnreset
                  (unix-error)
                  ((name :initform 'econnreset) (number :initform 104)
                   (description :initform "Connection reset by peer")))

(define-condition enobufs
                  (unix-error)
                  ((name :initform 'enobufs) (number :initform 105)
                   (description :initform "No buffer space available")))

(define-condition eisconn
                  (unix-error)
                  ((name :initform 'eisconn) (number :initform 106)
                   (description :initform
                                "Transport endpoint is already connected")))

(define-condition enotconn
                  (unix-error)
                  ((name :initform 'enotconn) (number :initform 107)
                   (description :initform
                                "Transport endpoint is not connected")))

(define-condition eshutdown
                  (unix-error)
                  ((name :initform 'eshutdown) (number :initform 108)
                   (description :initform
                                "Cannot send after transport endpoint shutdown")))

(define-condition etoomanyrefs
                  (unix-error)
                  ((name :initform 'etoomanyrefs) (number :initform 109)
                   (description :initform
                                "Too many references: cannot splice")))

(define-condition etimedout
                  (unix-error)
                  ((name :initform 'etimedout) (number :initform 110)
                   (description :initform "Connection timed out")))

(define-condition econnrefused
                  (unix-error)
                  ((name :initform 'econnrefused) (number :initform 111)
                   (description :initform "Connection refused")))

(define-condition ehostdown
                  (unix-error)
                  ((name :initform 'ehostdown) (number :initform 112)
                   (description :initform "Host is down")))

(define-condition ehostunreach
                  (unix-error)
                  ((name :initform 'ehostunreach) (number :initform 113)
                   (description :initform "No route to host")))

(define-condition ealready
                  (unix-error)
                  ((name :initform 'ealready) (number :initform 114)
                   (description :initform "Operation already in progress")))

(define-condition einprogress
                  (unix-error)
                  ((name :initform 'einprogress) (number :initform 115)
                   (description :initform "Operation now in progress")))

(define-condition estale
                  (unix-error)
                  ((name :initform 'estale) (number :initform 116)
                   (description :initform "Stale NFS file handle")))

(define-condition euclean
                  (unix-error)
                  ((name :initform 'euclean) (number :initform 117)
                   (description :initform "Structure needs cleaning")))

(define-condition enotnam
                  (unix-error)
                  ((name :initform 'enotnam) (number :initform 118)
                   (description :initform "Not a XENIX named type file")))

(define-condition enavail
                  (unix-error)
                  ((name :initform 'enavail) (number :initform 119)
                   (description :initform "No XENIX semaphores available")))

(define-condition eisnam
                  (unix-error)
                  ((name :initform 'eisnam) (number :initform 120)
                   (description :initform "Is a named type file")))

(define-condition eremoteio
                  (unix-error)
                  ((name :initform 'eremoteio) (number :initform 121)
                   (description :initform "Remote I/O error")))

(define-condition edquot
                  (unix-error)
                  ((name :initform 'edquot) (number :initform 122)
                   (description :initform "Quota exceeded")))

(define-condition enomedium
                  (unix-error)
                  ((name :initform 'enomedium) (number :initform 123)
                   (description :initform "No medium found")))

(define-condition emediumtype
                  (unix-error)
                  ((name :initform 'emediumtype) (number :initform 124)
                   (description :initform "Wrong medium type")))

(defvar *error-names*
  #(
    NIL
    eperm               ; Operation not permitted
    enoent              ; No such file or directory
    esrch               ; No such process
    eintr               ; Interrupted system call
    eio                 ; I/O error
    enxio               ; No such device or address
    e2big               ; Arg list too long
    enoexec             ; Exec format error
    ebadf               ; Bad file number
    echild              ; No child processes
    eagain              ; Try again
    enomem              ; Out of memory
    eacces              ; Permission denied
    efault              ; Bad address
    enotblk             ; Block device required
    ebusy               ; Device or resource busy
    eexist              ; File exists
    exdev               ; Cross-device link
    enodev              ; No such device
    enotdir             ; Not a directory
    eisdir              ; Is a directory
    einval              ; Invalid argument
    enfile              ; File table overflow
    emfile              ; Too many open files
    enotty              ; Not a typewriter
    etxtbsy             ; Text file busy
    efbig               ; File too large
    enospc              ; No space left on device
    espipe              ; Illegal seek
    erofs               ; Read-only file system
    emlink              ; Too many links
    epipe               ; Broken pipe
    edom                ; Math argument out of domain of func
    erange              ; Math result not representable
    edeadlk             ; Resource deadlock would occur
    enametoolong        ; File name too long
    enolck              ; No record locks available
    enosys              ; Function not implemented
    enotempty           ; Directory not empty
    eloop               ; Too many symbolic links encountered
    NIL
    enomsg              ; No message of desired type
    eidrm               ; Identifier removed
    echrng              ; Channel number out of range
    el2nsync            ; Level 2 not synchronized
    el3hlt              ; Level 3 halted
    el3rst              ; Level 3 reset
    elnrng              ; Link number out of range
    eunatch             ; Protocol driver not attached
    enocsi              ; No CSI structure available
    el2hlt              ; Level 2 halted
    ebade               ; Invalid exchange
    ebadr               ; Invalid request descriptor
    exfull              ; Exchange full
    enoano              ; No anode
    ebadrqc             ; Invalid request code
    ebadslt             ; Invalid slot
    NIL
    ebfont              ; Bad font file format
    enostr              ; Device not a stream
    enodata             ; No data available
    etime               ; Timer expired
    enosr               ; Out of streams resources
    enonet              ; Machine is not on the network
    enopkg              ; Package not installed
    eremote             ; Object is remote
    enolink             ; Link has been severed
    eadv                ; Advertise error
    esrmnt              ; Srmount error
    ecomm               ; Communication error on send
    eproto              ; Protocol error
    emultihop           ; Multihop attempted
    edotdot             ; RFS specific error
    ebadmsg             ; Not a data message
    eoverflow           ; Value too large for defined data type
    enotuniq            ; Name not unique on network
    ebadfd              ; File descriptor in bad state
    eremchg             ; Remote address changed
    elibacc             ; Can not access a needed shared library
    elibbad             ; Accessing a corrupted shared library
    elibscn             ; .lib section in a.out corrupted
    elibmax             ; Attempting to link in too many shared libraries
    elibexec            ; Cannot exec a shared library directly
    eilseq              ; Illegal byte sequence
    erestart            ; Interrupted system call should be restarted
    estrpipe            ; Streams pipe error
    eusers              ; Too many users
    enotsock            ; Socket operation on non-socket
    edestaddrreq        ; Destination address required
    emsgsize            ; Message too long
    eprototype          ; Protocol wrong type for socket
    enoprotoopt         ; Protocol not available
    eprotonosupport     ; Protocol not supported
    esocktnosupport     ; Socket type not supported
    eopnotsupp          ; Operation not supported on transport endpoint
    epfnosupport        ; Protocol family not supported
    eafnosupport        ; Address family not supported by protocol
    eaddrinuse          ; Address already in use
    eaddrnotavail       ; Cannot assign requested address
    enetdown            ; Network is down
    enetunreach         ; Network is unreachable
    enetreset           ; Network dropped connection because of reset
    econnaborted        ; Software caused connection abort
    econnreset          ; Connection reset by peer
    enobufs             ; No buffer space available
    eisconn             ; Transport endpoint is already connected
    enotconn            ; Transport endpoint is not connected
    eshutdown           ; Cannot send after transport endpoint shutdown
    etoomanyrefs        ; Too many references: cannot splice
    etimedout           ; Connection timed out
    econnrefused        ; Connection refused
    ehostdown           ; Host is down
    ehostunreach        ; No route to host
    ealready            ; Operation already in progress
    einprogress         ; Operation now in progress
    estale              ; Stale NFS file handle
    euclean             ; Structure needs cleaning
    enotnam             ; Not a XENIX named type file
    enavail             ; No XENIX semaphores available
    eisnam              ; Is a named type file
    eremoteio           ; Remote I/O error
    edquot              ; Quota exceeded
    enomedium           ; No medium found
    emediumtype         ; Wrong medium type
    NIL
    NIL
    NIL
    ))

(defvar *error-descriptions*
  #(
    NIL
    "Operation not permitted"                           ; eperm
    "No such file or directory"                         ; enoent
    "No such process"                                   ; esrch
    "Interrupted system call"                           ; eintr
    "I/O error"                                         ; eio
    "No such device or address"                         ; enxio
    "Arg list too long"                                 ; e2big
    "Exec format error"                                 ; enoexec
    "Bad file number"                                   ; ebadf
    "No child processes"                                ; echild
    "Try again"                                         ; eagain
    "Out of memory"                                     ; enomem
    "Permission denied"                                 ; eacces
    "Bad address"                                       ; efault
    "Block device required"                             ; enotblk
    "Device or resource busy"                           ; ebusy
    "File exists"                                       ; eexist
    "Cross-device link"                                 ; exdev
    "No such device"                                    ; enodev
    "Not a directory"                                   ; enotdir
    "Is a directory"                                    ; eisdir
    "Invalid argument"                                  ; einval
    "File table overflow"                               ; enfile
    "Too many open files"                               ; emfile
    "Not a typewriter"                                  ; enotty
    "Text file busy"                                    ; etxtbsy
    "File too large"                                    ; efbig
    "No space left on device"                           ; enospc
    "Illegal seek"                                      ; espipe
    "Read-only file system"                             ; erofs
    "Too many links"                                    ; emlink
    "Broken pipe"                                       ; epipe
    "Math argument out of domain of func"               ; edom
    "Math result not representable"                     ; erange
    "Resource deadlock would occur"                     ; edeadlk
    "File name too long"                                ; enametoolong
    "No record locks available"                         ; enolck
    "Function not implemented"                          ; enosys
    "Directory not empty"                               ; enotempty
    "Too many symbolic links encountered"               ; eloop
    NIL
    "No message of desired type"                        ; enomsg
    "Identifier removed"                                ; eidrm
    "Channel number out of range"                       ; echrng
    "Level 2 not synchronized"                          ; el2nsync
    "Level 3 halted"                                    ; el3hlt
    "Level 3 reset"                                     ; el3rst
    "Link number out of range"                          ; elnrng
    "Protocol driver not attached"                      ; eunatch
    "No CSI structure available"                        ; enocsi
    "Level 2 halted"                                    ; el2hlt
    "Invalid exchange"                                  ; ebade
    "Invalid request descriptor"                        ; ebadr
    "Exchange full"                                     ; exfull
    "No anode"                                          ; enoano
    "Invalid request code"                              ; ebadrqc
    "Invalid slot"                                      ; ebadslt
    NIL
    "Bad font file format"                              ; ebfont
    "Device not a stream"                               ; enostr
    "No data available"                                 ; enodata
    "Timer expired"                                     ; etime
    "Out of streams resources"                          ; enosr
    "Machine is not on the network"                     ; enonet
    "Package not installed"                             ; enopkg
    "Object is remote"                                  ; eremote
    "Link has been severed"                             ; enolink
    "Advertise error"                                   ; eadv
    "Srmount error"                                     ; esrmnt
    "Communication error on send"                       ; ecomm
    "Protocol error"                                    ; eproto
    "Multihop attempted"                                ; emultihop
    "RFS specific error"                                ; edotdot
    "Not a data message"                                ; ebadmsg
    "Value too large for defined data type"             ; eoverflow
    "Name not unique on network"                        ; enotuniq
    "File descriptor in bad state"                      ; ebadfd
    "Remote address changed"                            ; eremchg
    "Can not access a needed shared library"            ; elibacc
    "Accessing a corrupted shared library"              ; elibbad
    ".lib section in a.out corrupted"                   ; elibscn
    "Attempting to link in too many shared libraries"   ; elibmax
    "Cannot exec a shared library directly"             ; elibexec
    "Illegal byte sequence"                             ; eilseq
    "Interrupted system call should be restarted"       ; erestart
    "Streams pipe error"                                ; estrpipe
    "Too many users"                                    ; eusers
    "Socket operation on non-socket"                    ; enotsock
    "Destination address required"                      ; edestaddrreq
    "Message too long"                                  ; emsgsize
    "Protocol wrong type for socket"                    ; eprototype
    "Protocol not available"                            ; enoprotoopt
    "Protocol not supported"                            ; eprotonosupport
    "Socket type not supported"                         ; esocktnosupport
    "Operation not supported on transport endpoint"     ; eopnotsupp
    "Protocol family not supported"                     ; epfnosupport
    "Address family not supported by protocol"          ; eafnosupport
    "Address already in use"                            ; eaddrinuse
    "Cannot assign requested address"                   ; eaddrnotavail
    "Network is down"                                   ; enetdown
    "Network is unreachable"                            ; enetunreach
    "Network dropped connection because of reset"       ; enetreset
    "Software caused connection abort"                  ; econnaborted
    "Connection reset by peer"                          ; econnreset
    "No buffer space available"                         ; enobufs
    "Transport endpoint is already connected"           ; eisconn
    "Transport endpoint is not connected"               ; enotconn
    "Cannot send after transport endpoint shutdown"     ; eshutdown
    "Too many references: cannot splice"                ; etoomanyrefs
    "Connection timed out"                              ; etimedout
    "Connection refused"                                ; econnrefused
    "Host is down"                                      ; ehostdown
    "No route to host"                                  ; ehostunreach
    "Operation already in progress"                     ; ealready
    "Operation now in progress"                         ; einprogress
    "Stale NFS file handle"                             ; estale
    "Structure needs cleaning"                          ; euclean
    "Not a XENIX named type file"                       ; enotnam
    "No XENIX semaphores available"                     ; enavail
    "Is a named type file"                              ; eisnam
    "Remote I/O error"                                  ; eremoteio
    "Quota exceeded"                                    ; edquot
    "No medium found"                                   ; enomedium
    "Wrong medium type"                                 ; emediumtype
    NIL
    NIL
    NIL
    ))


(defun signal (number &optional from)
  (let ((type (aref *error-names* number)))
    (if type
        (error type :from from)
        (error 'unix-error
               :number number
               :name 'unknown-unix-error
               :from from
               :description (format nil "Unknown Unix error ~D" number)))))

(defun errno ()
  (sb-alien:get-errno))

(defun perror (code &optional prefix)
  (format nil "~:[~;~A: ~]~A" prefix (aref *error-descriptions* code)))

