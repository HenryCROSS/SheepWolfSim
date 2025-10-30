(in-package #:tools)

(defun helper-fn ()
  "工具模块中的辅助函数。"
  (format t "[tools] 正在执行辅助函数。~%")
  ;; 示例逻辑：可以是日志、配置加载、调试工具等
  (let ((timestamp (get-universal-time)))
    (format t "[tools] 当前时间戳：~A~%" timestamp)))
