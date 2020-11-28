(defsystem "simple-log"
  :class :package-inferred-system
  :description "minimalistic CL logger"

:long-description
#.(uiop:read-file-string (uiop/pathname:subpathname *load-pathname* "description.org"))
  :author "Oleg Shalaev"
  :mailto "oleg@chalaev.com"
  :licence "MIT"
  :version (:read-file-line "version.org")
  :depends-on (:bordeaux-threads :local-time :uiop :shalaev/macros)
  :components ((:file "simple-log"))
  :in-order-to ((test-op (test-op "simple-log/tests"))))

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(defsystem "simple-log/example"
:class :package-inferred-system
:depends-on (:simple-log :shalaev/macros)

:build-operation  "program-op"
:build-pathname "example.bin"
:entry-point "simple-log/example:main"

:description "a compilable example for simple-log"
:author "Oleg Shalaev"
:mailto "oleg@chalaev.com"
:licence "MIT"
:version (:read-file-line "version.org")
:components ((:file "example")))

(defsystem "simple-log/tests"
  :class :package-inferred-system
  :description "testing"
  :author "Oleg Shalaev"
  :mailto "oleg@chalaev.com"
  :licence "MIT"
  :version (:read-file-line "version.org")
  :depends-on (:simple-log :sb-rt)
  :components ((:file "tests"))
  :perform (test-op (o c)
(flet ((run-tests (&rest args)
         (apply (intern (string '#:run-tests) '#:simple-log/tests) args)))
  (run-tests :compiled nil)
  (run-tests :compiled t))))
