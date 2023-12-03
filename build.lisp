(load "init.lisp")
(let ((filename
	(namestring (make-pathname :directory '(:relative "dist")
		       :name (or (second *posix-argv*) "ledger")))))
  (sb-ext:save-lisp-and-die filename :toplevel #'main :executable t)
  )
