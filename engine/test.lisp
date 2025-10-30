(in-package #:engine)

(defun run-e ()
  "启动引擎的主逻辑。"
  (format t "[engine] 测试已启动。~%")
  ;; 示例逻辑：可以替换为调度器、状态机或游戏循环等
  (loop for i from 1 to 3 do
       (format t "[engine] Tick ~A~%" i))
  (format t "[engine] 测试运行结束。~%"))
