(defpackage :engine
  (:use :cl :sdl2)
  (:shadowing-import-from :cl-opengl :create-texture)
  (:export :run
           :run-e))