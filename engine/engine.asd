(defsystem "engine"
           :depends-on (:sdl2 :cl-opengl)
           :components
           ((:file "package")
            (:file "engine-core")
            (:file "test")))
