(defpackage :simple-log/tests
  (:use :cl :shalaev/macros :sb-rt)
  (:export :N-failed)
  (:import-from :sb-rt :*compile-tests* :*expected-failures*))
(in-package :simple-log/tests)

(defvar N-failed 0 "how many tests failed")

(defun run-tests (&key ((:compiled *compile-tests*)))
  (unless (do-tests) (incf N-failed)))
