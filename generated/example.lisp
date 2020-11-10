(declaim (optimize (speed 3) (safety 0)))
(defpackage :simple-log/example
  (:shadow cl:warning cl:debug cl:log)
  (:export :main)
  (:use :cl :simple-log))
(in-package :simple-log/example)

(defun log-SLU (type &rest message)
  (apply #'SL:log
    (cons type (cons (concatenate 'string "SLU " (car message)) (cdr message)))))

(defun log-SLD (type &rest message)
  (apply #'SL:log
    (cons type (cons (concatenate 'string "SLD " (car message)) (cdr message)))))

(defun main()
  (setf simple-log:out-streams (list *standard-output*))

(log-SLU SL:warning "this is a warning from the log service #~d" 1)
(log-SLD SL:info "this is the ~ath info from the log service #~d" 146 2)
(sleep 2.345)
(log-SLU SL:warning "this is a warning from the log service #~d" 3)
(sleep 0.111)
(log-SLD SL:info "this is the ~ath info from the log service #~d" 137 4)
(SL:stop))
