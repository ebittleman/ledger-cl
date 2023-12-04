(load "vendor/setup.lisp")
(push (uiop/os:getcwd) asdf:*central-registry*)

(ql:quickload "ledger-cl")
