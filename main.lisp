(use-package 'engine)
(use-package 'tools)

(in-package #:sheepwolfsim)


(defun main ()
  (format t "[main] 项目启动中...~%")
  (engine:run-e)
  (tools:helper-fn)
  (engine:run)
  (format t "[main] 项目执行完毕。~%")
  (tools:helper-fn))
