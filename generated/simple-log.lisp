(defvar log-types (list 'debug 'info 'warning 'error))
(loop for field-name in log-types for i from 0 do (set field-name i))
(declaim (type (integer) *maxLogLevel* level))
(defvar *maxLogLevel* (1- (length log-types)))
(defvar level 0 "default (minimal) value")

(let((ms(/ internal-time-units-per-second 1000)))
(defun ms(internal-real-time)
  (round(/ internal-real-time ms))))

(defvar out-streams nil "list of auxillary streams for extra log copies")
(defvar dir nil "directory where log files will be stored")
(defvar log-file nil "log file name, required before we load the package")
(declaim (type (integer) *maxLogLevel* level))
(defvar *start-time* nil)
(defvar *queue-lock* (bt:make-lock) "we need locks on multi-threading systems")
(defvar *tobe-printed* nil "log messages buffer")

(defvar *print-timer* nil "needed for flushing the log every second")

(defun format-msg(stream msg &optional (ST *start-time*))
;; (declare (integer s))
(multiple-value-bind (s ms) (floor (- (cadr msg) (cdr ST)) 1000)

(multiple-value-bind (s mi h d mo) (decode-universal-time (+ (car ST) s))
(loop for str in (cons stream out-streams) do

(let ((message (third msg)))
  (apply #'format (append (list str

(concat
"~&~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d.~3,'0d ~a " (car message))

mo  d  h mi s ms
(nth (first msg) log-types)); e.g., INFO
(cdr message))))
(format str "~%")))))

(defun printer()
"flushes the log buffer"
(when *tobe-printed* (bt:with-lock-held (*queue-lock*)
(with-open-file(s log-file :direction :output :if-exists :append :if-does-not-exist :create)
  (mapcar #'(lambda(msg) (format-msg s msg)) (reverse *tobe-printed*))
  (setf *tobe-printed* nil)))))

(defun log (livello &rest message)
"main log function"
;;(declare (integer livello)) ;(declare (type integer livello))

(ifn (start) (format t "could not start the logging system")

(bt:with-lock-held (*queue-lock*)
  (when (<= level livello)
    (push (list livello (ms(get-internal-real-time)) message) *tobe-printed*)))))

(defvar default-log-dir (find-if #'uiop:directory-exists-p '("/var/log/sbcl/" "/tmp/")))
(defun start(&optional dir FN)
  (iff *print-timer* t
    (setf dir (if (stringp dir) dir default-log-dir)
          log-file (merge-pathnames
                    (if (stringp FN) FN "server.log")
                    dir))
    (ifn (uiop:directory-exists-p dir)
	 (format t "refuse to start because ~a does not exist, please create it~%" dir)

(setf *start-time* (cons (get-universal-time) (ms(get-internal-real-time)))
      *print-timer* (sb-ext:make-timer #'printer :thread t))
(sb-ext:schedule-timer *print-timer* 1 :repeat-interval 1) t)))

(defun stop()
(when (and *print-timer* (sb-ext:timer-scheduled-p *print-timer*))
  (sb-ext:unschedule-timer *print-timer*))
  (setf *print-timer* nil)
  (printer))
