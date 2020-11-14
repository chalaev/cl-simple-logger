(asdf:defsystem "simple-log"
  :description "minimalistic CL logger"

:long-description
#.(uiop:read-file-string (uiop/pathname:subpathname *load-pathname* "description.org"))
  :author "Oleg Shalaev"
  :mailto "oleg@chalaev.com"
  :licence "MIT"
  :version "0"
  :depends-on (:bordeaux-threads :local-time :osicat :uiop)
  :serial t
  :components ((:file "simple-log")))

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(asdf:defsystem "simple-log/example"
:depends-on (:simple-log)

:build-operation  "program-op"
:build-pathname "example.bin"
:entry-point "simple-log/example:main"

:description "an example for simple-log"
:author "Oleg Shalaev"
:mailto "oleg@chalaev.com"
:licence "MIT"
:version "0"
:components ((:file "example")))
