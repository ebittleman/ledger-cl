(in-package #:ledger-cl)

(defun main ()
  (format t "hello, value: ~a~%" (ledger-cl:wrap 5)))
