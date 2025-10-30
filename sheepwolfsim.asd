(defsystem "sheepwolfsim"
           :description "A 2D simulation project"
           :author "128bit devs"
           :version "1.0"
           :license "GPL"
           :depends-on ("engine" "tools")
           :components ((:file "package")
                        (:file "main")))
