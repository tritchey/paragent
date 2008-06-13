(require 'asdf)
(require 'asdf-binary-locations)

(asdf:oos 'asdf:load-op 'adjutant)
(in-package :adjutant)
(loop-endlessly)

