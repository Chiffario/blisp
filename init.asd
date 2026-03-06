(defsystem "blisp"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:alexandria
               :hunchentoot
               :spinneret
               :cl-markdown)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "")
