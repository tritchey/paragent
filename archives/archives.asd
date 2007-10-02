;;; -*- Lisp -*-
;;;; Paragent Confidential
;;;;
;;;; Nexus
;;;; (C) Copyright Paragent, LLC 2007
;;;;
;;;;
;;;; The source code for this program is not published or otherwise divested
;;;; of its trade secrets, irrespective of what has been deposited with the
;;;; U.S. Copyright office.
;;;;
;;;; All Rights Reserved. Use, duplication or disclosure restricted unless
;;;; granted by formal written contract with Paragent, LLC.

 (require 'asdf-binary-locations) 

(defpackage #:archives-asd
  (:use :cl :asdf))

(in-package :archives-asd)

(defsystem archives
  :name "archives"
  :version "0.1"
  :serial t
  :components ((:file "defpackage")
               (:file "main" ))
  :depends-on (:asdf-binary-locations :db :mel-base :cl-ppcre :cl-smtp))

