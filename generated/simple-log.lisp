;; generated from .org
(declaim (optimize (speed 3) (safety 0)))
(defpackage :simple-log
(:shadow cl:warning cl:debug cl:log cl:error)
(:nicknames "SL")
(:use :cl)
(:export :log :out-streams :level :start :stop :debug :info :warning :error))
(in-package :simple-log)
(eval-when (:compile-toplevel)
  (loop for field-name in '(debug info warning error) for i from 0 do (defvar field-name i))
  (let ((goodies/ (uiop:ensure-directory-pathname "goodies")))
    (mapcar #'(lambda(FN) (load (merge-pathnames FN goodies/)))
      '(#p"macros.lisp"))))

(defvar log-types (list 'debug 'info 'warning 'error))
(loop for field-name in log-types for i from 0 do (set field-name i))
(defvar *maxLogLevel* (1- (length log-types)))
(defvar level 0 "default (minimal) value")
;;(declare (integer level))

(defvar out-streams nil "list of auxillary streams for extra log copies")
(defvar dir nil "directory where log files will be stored")
(defvar log-file nil "log file name, required before we load the package")
(defvar *start-time* nil "needed to figure out the timing")
(defvar *queue-lock* (bt:make-lock) "we need locks when running on multi-threading systems")
(defvar *tobe-printed* nil "log buffer")

(defvar *print-timer* nil "needed for flushing the log every second")

(defun format-msg (stream msg)
(let ((dt (- (second msg) (cdr *start-time*))))
(declare (integer dt))
  (multiple-value-bind (s fractions) (floor (+ (car *start-time*) dt) internal-time-units-per-second)
    (let ((ms (floor fractions (floor internal-time-units-per-second 1000))))
    (multiple-value-bind (s mi h d mo) (decode-universal-time s)
(loop for str in (cons stream out-streams) do

(let ((message (third msg)))
  (apply #'format (append (list str

(concat
"~&~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d.~3,'0d ~a " (car message))

mo  d  h mi s ms
(nth (first msg) log-types)); e.g., INFO
(cdr message))))
(format str "~%")))))))

(defun printer()
"flushes the log buffer"
(when *tobe-printed* (bt:with-lock-held (*queue-lock*)
(with-open-file (s log-file :direction :output :if-exists :append)
  (mapcar #'(lambda(msg) (format-msg s msg)) (reverse *tobe-printed*))
  (setf *tobe-printed* nil)))))

(defun log (livello &rest message)
"main log function"
;;(declare (integer livello)) ;(declare (type integer livello))

(ifn (start) (format t "could not start the logging system")

(bt:with-lock-held (*queue-lock*)
  (when (<= level livello)
    (push (list livello (get-internal-real-time) message) *tobe-printed*)))))

(defvar default-log-dir (find-if #'uiop:directory-exists-p '("/var/log/sbcl/" "/tmp/")))
(defun start(&optional dir FN)
  (iff *print-timer* t
    (setf dir (if (stringp dir) dir default-log-dir)
          log-file (merge-pathnames
                    (if (stringp FN) FN "server.log")
                    dir))
    (ifn (uiop:directory-exists-p dir)
	 (format t "refuse to start because ~a does not exist, please create it~%" dir)

(setf *start-time* (cons (* internal-time-units-per-second (get-universal-time)) (get-internal-real-time))
      *print-timer* (sb-ext:make-timer #'printer :thread t))
(sb-ext:schedule-timer *print-timer* 1 :repeat-interval 1) t)))

(defun stop()
(when (and *print-timer* (sb-ext:timer-scheduled-p *print-timer*))
  (sb-ext:unschedule-timer *print-timer*))
  (setf *print-timer* nil)
  (printer))
