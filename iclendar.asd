#|
 This file is a part of iclendar
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem iclendar
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An iCalendar format lirbary."
  :homepage "https://github.com/Shinmera/iclendar"
  :serial T
  :components ((:file "package")
               (:file "protocol")
               (:file "standard-types")
               (:file "standard-parameters")
               (:file "standard-blocks")
               (:file "documentation"))
  :depends-on (:closer-mop
               :documentation-utils))
