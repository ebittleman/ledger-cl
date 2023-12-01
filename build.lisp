(load (compile-file "main.lisp"))
(sb-ext:save-lisp-and-die "hello" :toplevel #'main :executable t)
