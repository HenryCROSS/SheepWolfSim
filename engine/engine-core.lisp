(in-package #:engine)

#|
(defun debug-log (msg &rest args)
  "Output and flush MSG to STDOUT with arguments ARGS"
  (apply #'format t msg args)
  ;; Flush to standard out
  (finish-output))

(defun setup-gl (win gl-context)
  "Setup OpenGL with the window WIN and the gl context of GL-CONTEXT"
  (debug-log "Setting up window/gl.~%")
  (sdl2:gl-make-current win gl-context)
  (gl:viewport 0 0 800 600)
  (gl:matrix-mode :projection)
  (gl:ortho -2 2 -2 2 -2 2)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  ;; Clear to black
  (gl:clear-color 0.0 0.0 0.0 1.0))

(defun render ()
  (gl:clear :color-buffer)
  ;; Draw a demo triangle
  (gl:begin :triangles)
  (gl:color 1.0 0.0 0.0)
  (gl:vertex 0.0 1.0)
  (gl:vertex -1.0 -1.0)
  (gl:vertex 1.0 -1.0)
  (gl:end)
  (gl:flush))

(defun main-loop (win render-fn)
  "Run the game loop that handles input, rendering through the
  render function RENDER-FN, amongst others."
  (sdl2:with-event-loop (:method :poll)
    (:idle ()
           (funcall render-fn)
           ;; Swap back buffer
           (sdl2:gl-swap-window win))
    (:quit () t)))

(defun run ()
  "The entry point of our game."
  (sdl2:with-init (:everything)
    (debug-log "Using SDL library version: ~D.~D.~D~%"
      sdl2-ffi:+sdl-major-version+
      sdl2-ffi:+sdl-minor-version+
      sdl2-ffi:+sdl-patchlevel+)

    (sdl2:with-window (win :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
        ;; Basic window/gl setup
        (setup-gl win gl-context)

        ;; Run main loop
        (main-loop win #'render)))))
|#

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)
(defparameter *screen-title* "Sheep & Wolf")

(defmacro with-opengl-window ((window gl-context) &body body)
  `(sdl2:with-init (:everything)
     (sdl2:with-window (,window
                         :title *screen-title*
                         :w *screen-width*
                         :h *screen-height*
                         :flags '(:shown :opengl))
       (sdl2:with-gl-context (,gl-context ,window)
         ,@body))))

(defun setup-gl (win gl-context)
  "Setup OpenGL with the window WIN and the gl context of GL-CONTEXT"
  (format t "Setting up window/gl.~%")
  (sdl2:gl-make-current win gl-context)
  (gl:viewport 0 0 800 600)
  (gl:matrix-mode :projection)
  (gl:ortho -2 2 -2 2 -2 2)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  ;; Clear to black
  (gl:clear-color 0.0 0.0 0.0 1.0))

(defparameter *r* 1.0)
(defparameter *g* 1.0)
(defparameter *b* 1.0)

(defun render ()
  (gl:clear :color-buffer)
  ;; Draw a demo triangle
  (gl:begin :triangles)
  (gl:color *r* *g* *b*)
  (gl:vertex 0.0 1.0)
  (gl:vertex -1.0 -1.0)
  (gl:vertex 1.0 -1.0)
  (gl:end)
  (gl:flush))

(defun main-loop (window render-fn)
  (sdl2:with-event-loop
    (:method :poll)
    (:quit () t)
    (:keydown
     (:keysym keysym)
     (case (sdl2:scancode keysym)
       (:scancode-q (sdl2:push-quit-event))
       (:scancode-r
        (progn
         (setf *r* (random 1.0)
           *g* (random 1.0)
           *b* (random 1.0))
         (print (list :r *r* :g *g* :b *b*))))))
    (:idle ()
           (funcall render-fn)
           (sdl2:gl-swap-window window)
           (sdl2:delay 100))))

(defun run ()
  (with-opengl-window (window gl-context)
    (setup-gl window gl-context)
    (main-loop window #'render)))