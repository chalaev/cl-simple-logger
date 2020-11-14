;; generated from .org
(declaim (optimize (speed 3) (safety 0)))
(defpackage :simple-log/example
  (:export :main)
  (:use :cl))
(in-package :simple-log/example)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((goodies/ (uiop:ensure-directory-pathname "goodies")))
    (mapcar #'(lambda(FN) (load (merge-pathnames FN goodies/)))
      '(#p"macros.lisp"))))

(defun log-SLU (type &rest message)
  (apply #'SL:log
    (cons type (cons (concat "SLU " (car message)) (cdr message)))))
(defun log-SLD (type &rest message)
  (apply #'SL:log
    (cons type (cons (concat "SLD " (car message)) (cdr message)))))

(defun main()
  (setf simple-log:out-streams (list *standard-output*))

(log-SLU sl:warning "this is a warning from the log service #~d" 1)
(log-SLD sl:info "this is the ~ath info from the log service #~d" 146 2)
(sleep 2.345)
(log-SLU SL:error "this is an error from the log service #~d" 3)
(sleep 0.111)
(log-SLD SL:info "this is the ~ath info from the log service #~d" 137 4)
(SL:stop))
