;;(declaim (optimize (speed 3) (safety 0)))
(defpackage :simple-log/example
  (:shadow cl:warning cl:debug cl:log cl:error)
  (:export :main)
  (:use :cl :shalaev/macros :simple-log))
(in-package :simple-log/example)
