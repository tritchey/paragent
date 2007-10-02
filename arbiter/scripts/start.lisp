(require 'asdf)
(require 'asdf-binary-locations)

(asdf:oos 'asdf:load-op 'arbiter)
(in-package :arbiter)
(set-rlimit-nofile 8192)
(init)

