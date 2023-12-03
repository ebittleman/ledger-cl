(load "vendor/setup.lisp")
(push (uiop/os:getcwd) asdf:*central-registry*)

(load "src/main.lisp")
