;;;; ledger-cl.asd
(asdf:defsystem #:ledger-cl
  :name "ledger-cl"
  :description "Common general journal and ledger library"
  :license "MIT"
  :author "Eric Bittleman<eric.bittleman@gmail.com>"
  :depends-on (#:parse-float #:uuid)
  :components ((:module "src"
		:components
		((:file "package")
		 (:file "journal" :depends-on ("package"))
		 (:file "main" :depends-on ("journal"))
		 )))
  :build-operation program-op
  :build-pathname "dist/ledger"
  :entry-point "ledger-cl::main")
