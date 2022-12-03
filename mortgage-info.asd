(defsystem :mortgage-info
  :author "Jonathan Godbout"
  :version "0.0.1"
  :licence "MIT-style"
  :description      "Functions for mortgage info"
  :long-description "Functions for mortgage info"
  :defsystem-depends-on (:cl-protobufs)
  :depends-on (:hunchentoot :cl-protobufs :grpc :ace.flag)
  :components
  ((:module "src"
    :serial t
    :pathname ""
    :components ((:protobuf-source-file "mortgage")
                 (:file "mortgage-info")))))
