;; (load "./../vendor/quicklisp/setup.lisp")

;; (ql:quickload "parse-float")

;; (use-package :parse-float)

;(defun filename ()
;  (format t "compile: ~a~%" (or *compile-file-truename* *load-truename*))
;  (format t "load: ~a~%" (or *load-pathname* *load-truename*))
;)



(load "./vendor/quicklisp/setup.lisp")

(ql:quickload "parse-float")
(require "parse-float")
(require "asdf")
(push (uiop/os:getcwd) asdf:*central-registry*)
(asdf:asdf-version)
(require "ledger-cl")
