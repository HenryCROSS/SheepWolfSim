(load (merge-pathnames "quicklisp/setup.lisp"
                       (user-homedir-pathname)))

(ql:quickload :sheepwolfsim)

(sb-ext:save-lisp-and-die "sheepwolfsim.exe"
  :toplevel #'sheepwolfsim:main
  :executable t
  :compression nil)
