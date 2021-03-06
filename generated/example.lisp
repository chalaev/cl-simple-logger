(defun log-SLU (log-type &rest message)
  (apply #'SL:log
    (cons log-type (cons (concat "SLU " (car message)) (cdr message)))))
(defun log-SLD (log-type &rest message)
  (apply #'SL:log
    (cons log-type (cons (concat "SLD " (car message)) (cdr message)))))

(defun main()
  (setf simple-log:out-streams (list *standard-output*))

(log-SLU sl:warning "this is a warning from the log service #~d" 1)
(log-SLD sl:info "this is the ~ath info from the log service #~d" 146 2)
(sleep 2.345)
(log-SLU SL:error "this is an error from the log service #~d" 3)
(sleep 0.111)
(log-SLD SL:info "this is the ~ath info from the log service #~d" 137 4)
(SL:stop))
