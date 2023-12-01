;;;; ledger-cl.asd

(asdf:defsystem #:ledger-cl
  :name "ledger-cl"
  :description "Common general journal and ledger library"
  :license "MIT"
  :author "Eric Bittleman<eric.bittleman@gmail.com>"
  ;; :depends-on (#'parse-float)
  :components ((:file "src/package")
	       (:file "src/journal" :depends-on ("src/package"))))

