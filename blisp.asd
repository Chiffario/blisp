(defsystem "blisp"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:alexandria
               :hunchentoot
               :spinneret
               :cl-markdown
               :fxml
               :str)
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "templating")
                 (:file "markdown")
                 (:file "feed")
                 (:file "routing"))))
  :description "")
