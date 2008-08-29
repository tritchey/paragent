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

(defpackage #:nexus-asd
  (:use :cl :asdf))

(in-package :nexus-asd)

(defsystem nexus
  :name "nexus"
  :version "0.1"
  :serial t
  :components ((:file "defpackage")
               (:file "app" )
               (:file "ucw-custom")
               (:file "pages")
               (:file "functions")
               (:file "components" :pathname "components/components.lisp")
	       (:file "drop-down" :pathname "components/drop-down.lisp")
               (:file "item-list" :pathname "components/item-list.lisp")
               (:file "tabbed-view" :pathname "components/tabbed-view.lisp")
	       (:file "png")
	       (:file "cl-vector-chart" :depends-on ("png"))
	       (:file "charts" :depends-on ("cl-vector-chart"))
	       (:file "worm-charts" :depends-on ("charts"))
               (:file "events")
               (:file "utils")
               (:file "queue")
               (:file "message-broker")
               (:file "archon")
               (:file "login")
               (:file "prop-display" :pathname "reports/prop-display.lisp")
               (:file "logged-errors")
               (:file "main")
               (:file "preferences" :pathname "preferences/preferences.lisp")
               (:file "preferences-groups" :pathname "preferences/groups.lisp")
               (:file "preferences-tickets" :pathname "preferences/tickets.lisp")
               (:file "preferences-users" :pathname "preferences/users.lisp")
               (:file "preferences-uninstall" :pathname "preferences/uninstall.lisp")
               (:file "software")
               (:file "computer" :pathname "computers/computer.lisp")
               (:file "computers" :pathname "computers/computers.lisp")
               (:file "alert-form" :pathname "alerts/alert-form.lisp")
               (:file "alert" :pathname "alerts/alert.lisp")	       
               (:file "alerts" :pathname "alerts/alerts.lisp")
               (:file "tickets" :pathname "tickets/tickets.lisp")
               (:file "ticket-form" :pathname "tickets/ticket-form.lisp")
               (:file "ticket-dialogs" :pathname "tickets/ticket-dialogs.lisp")
               (:file "ticket" :pathname "tickets/ticket.lisp")
               (:file "reports" :pathname "reports/reports.lisp")
               (:file "ticket-reports" :pathname "reports/ticket-reports.lisp")
               (:file "warranty-report" :pathname "reports/warranty-report.lisp")
               (:file "search" :pathname "search/search.lisp")
               (:file "advanced-search" :pathname "search/advanced.lisp")
               (:file "inventory" :pathname "reports/inventory.lisp")
               (:file "admin")
               (:file "credit-card" :pathname "signup/credit-card.lisp")
               (:file "signup" :pathname "signup/signup.lisp")
               (:file "observer")
	       ;(:file "db-admin" :pathname "database/db-admin.lisp")
               )
  :depends-on (
	       :db 
	       :ironclad 
	       :cl-vectors 
	       :cl-paths-ttf 
	       :salza 
	       :zpb-ttf
	       :wormtrails
               :drakma
	       :psi))

