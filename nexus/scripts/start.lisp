(require 'asdf)
(require 'asdf-binary-locations)

(load "/lisp/ucw-boxset/start")
;(setf ucw:*debug-on-error* t)
(asdf:oos 'asdf:load-op 'nexus)
