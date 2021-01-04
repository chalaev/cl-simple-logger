;;(declaim (optimize (speed 3) (safety 0)))
(defpackage :simple-log
(:shadow cl:warning cl:debug cl:log cl:error)
(:nicknames "SL")
(:use :cl :shalaev/macros)
(:export :ms :log :out-streams :level :start :stop :debug :info :warning :error))
(in-package :simple-log)
(eval-when (:compile-toplevel)
  (loop for field-name in '(debug info warning error) for i from 0 do (defvar field-name i)))
