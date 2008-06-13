;;;; Created on 2006-12-07 14:53:15



(defpackage #:adjutant-asd
  (:use :cl :asdf))

(in-package :adjutant-asd)

(defsystem adjutant
  :name "adjutant"
  :version "0.1"
  :components ((:file "defpackage")
               (:file "main" :depends-on ("defpackage")))
  :depends-on (:asdf-binary-locations :db :cl-smtp))


  

