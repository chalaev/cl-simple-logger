(ql:quickload :simple-log)

;; Suppose we have a web server with several lisp-powered) services;
;; each service is a package depending on :simple-log.

;; These services are independent, probably run in different threads, and can send their log messages any time.

;; The first service will use log-SLU as a log function:
(defun log-SLU (type &rest message)
  (apply #'SL:log
    (cons type (cons (concatenate 'string "SLU " (car message)) (cdr message)))))
;; -- we distinguish its log messages from the other ones thanks to the "SLU"-prefix.

;; Similarly, another service will use log-SLD as a log function:
(defun log-SLD (type &rest message)
  (apply #'SL:log
    (cons type (cons (concatenate 'string "SLD " (car message)) (cdr message)))))

;; apart from the log file, we may want to receive log messages in the terminal:
(setf simple-log:out-streams (list *standard-output*))

(log-SLU SL:warning "this is a warning from the log service #~d" 1)

(log-SLD SL:info "this is the ~ath info from the log service #~d" 146 2)

(sleep 2.345)

(log-SLU SL:warning "this is a warning from the log service #~d" 1)

(sleep 0.111)

(log-SLD SL:info "this is the ~ath info from the log service #~d" 137 2)

(SL:stop)

;; and in case you need to calculate a logarithm, use
(cl:log 2.8)
