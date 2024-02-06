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
		 (:file "common" :depends-on ("package"))
		 (:file "ledger" :depends-on ("common"))
		 (:file "journal" :depends-on ("ledger"))
		 (:file "inventory" :depends-on ("journal"))
		 (:file "state" :depends-on ("inventory"))
		 (:file "main" :depends-on ("state"))
		 )))
  :build-operation program-op
  :build-pathname "dist/ledger"
  :entry-point "ledger-cl::main")
