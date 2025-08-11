(asdf:defsystem iclendar
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "An iCalendar format lirbary."
  :homepage "https://shinmera.com/docs/iclendar/"
  :bug-tracker "https://shinmera.com/project/iclendar/issues"
  :source-control (:git "https://shinmera.com/project/iclendar.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "stream")
               (:file "protocol")
               (:file "standard-types")
               (:file "standard-parameters")
               (:file "standard-properties")
               (:file "standard-components")
               (:file "serializer")
               (:file "documentation"))
  :depends-on (:closer-mop
               :cl-base64
               :trivial-gray-streams
               :documentation-utils))
