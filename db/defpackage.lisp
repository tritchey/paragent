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

(clsql:push-library-path "/usr/lib/mysql/")
;(clsql:push-library-path *db-library-path*)
(asdf:oos 'asdf:load-op 'clsql-mysql)
                         

(defpackage :com.paragent.db
  (:nicknames :db)
  (:use :cl :sb-thread :clsql)
  (:export 
    :with-db :db-connect :equiv :strict-equiv :table-for-object :severity-name
    :with-restarting-transaction :sql-equal
    :fill-with-test-data :type-alerts :hash-password
    :software-event :installed :created :trial-enabled-p
   :user :company :computer :event :event-type :severity-id :severity :level
   :operating-system :processor :memory :bios :cd-rom :hard-drive 
   :logical-drive :logical-drives :free-space :video-controller
   :sound-device :network-card :system :printer :software :software-computer-link
   :startup :hot-fix :hardware-error :process :service :user-account
   :get-last-insert :insert-and-update :name=
   :software-id :license-key :msi :secret
   :computer-tag :tags :email
   :alert :type-id :email-to :args :alerts :computers :impl :enabled 
   :alert-computer-link :alert-event-link :event-id :alert-id
   :*alert-types* :arg1 :db-obj
   :+alert-cd+ :+alert-hard-drive+ :+alert-processor+ :+alert-memory+ 
   :+alert-software+
   :+alert-service-stop+ :+alert-service-start+ :+alert-process-stop+ 
   :+alert-computer-offline+
   :+alert-pnp-added+ :+alert-pnp-removed+ :+alert-user-logon+ :+alert-user-logoff+
   :alert-cd :alert-hard-drive :alert-processor :alert-memory :alert-software
   :alert-service-stop :alert-service-start :alert-process-stop 
   :alert-computer-offline
   :alert-pnp-added :alert-pnp-removed
   :alert-user-logon :alert-user-logoff
   :system-note :templar-update :begin :end :files
   :license-scheme :license-scheme-id :license-counts :license-count :num
   :logged-last-login :last-login :weekly-software-report
   :ip-address :ip-addresses
   :id :company-id :username :password :name :alias :recent-computers :users :computers
   :motherboard :motherboards
   :archon-connection :online :bios :cd-roms :hard-drives :logical-drives 
   :hardware-errors
   :note :warranty :last-online
   :processors :user-accounts :locked :disabled
   :hot-fixes :memory :network-cards :printers :processes
   :services :sound-devices :startups :systems :video-controllers
   :events :last-refresh :computer-id :summary :description :timestamp
   :severity-type :product-id :version :registered-user :service-pack
   :info :architecture :l2-cache :clock-speed :capacity
   :memory-speed :form-factor :language :manufacturer 
   :location :max-capacity :num-slots :error-correction
   :memory-array
   :+memory-error-correction-reserved+ :+memory-error-correction-other+ 
   :+memory-error-correction-unknown+ :+memory-error-correction-none+
   :+memory-error-correction-parity+ :+memory-error-correction-single-ecc+
   :+memory-error-correction-multi-ecc+ :+memory-error-correction-crc+
   :error-correction-string
   :firewall :antivirus :up-to-date :release-date
   :serial-number :drive :size :free-space :interface-type
   :horizontal-resolution :vertical-resolution :refresh-rate
   :driver :manufacturer :domain :mac-address :ip
   :is-default :publisher :location :command :user :description
   :installed-by :install-date
    :+account-free+ :+account-basic+ :+account-plus+ :+account-premium+
    :>=account-basic :>=account-plus :>=account-premium :=account-free
   :+ticket-status-closed+ :+ticket-status-open+ :+ticket-status-hold+
   :+ticket-priority-note+ :+ticket-priority-low+ :+ticket-priority-medium+
   :+ticket-priority-high+ :+ticket-priority-critical+
   :ticket :tickets :open-tickets :open-tickets-count
   :email-smtp :authp
   :state :priority :subject :response-email :rating :assigned-user :assigned-user-id
   :body :time-spent :priority-name :ticket-id :company-ticket-id :next-ticket-id
    :ticket-comment :ticket-computer :ticket-response :ticket-tag :recent-ticket :recent-tickets
    :due-date :ticket-change :get-next-ticket-id
    :ticket-email :ticket-emails :host :port :sender
    :group :groups :group-id :group-computer-link :group-user-link :users :user-id
    :remote-permission :shutdown-permission :note-permission :all-computers
    :user-session :session-id :generate-session-id :expiration
    :+event-online+ :+event-offline+ :+event-hardware-add+ :+event-hardware-remove+
    :+event-software-add+ :+event-software-remove+ :+event-hotfix-add+ :+event-hotfix-remove+ 
    :+event-alert+ :+event-service-add+ :+event-service-remove+ :+event-user-add+
    :+event-user-remove+ :+event-hardware-error-add+ :+event-hardware-error-remove+
    :+event-remote-desktop+
    :scarab-message
    :logged-error
    :subscription :num-computers :commitment :phone-number :subscription-id
    :*db-database* :*db-username* :*db-password* :*email-from-address* :*email-server*
    :*email-smtp-authentication* :*email-account-name* :*email-account-password*
    :send-email :send-ticketing-email :*server-url* :*administrator-notification-email* 
    :record :*record-preamble*
    :*credit-card-login* :*credit-card-password*
    :*templar-ssl-cert* :*templar-ssl-key*
    :*default-arbiter-templar-port* :*default-arbiter-nexus-port*
    :*default-arbiter-archon-port* :*default-arbiter-address* :*default-dark-archon-server*
))

