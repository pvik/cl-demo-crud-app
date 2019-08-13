(defsystem "cl-demo-crud-app"
  :version "0.1.0"
  :author "pvik"
  :license "MIT"
  :depends-on ("postmodern"
			   "local-time")
  :components ((:module "src"
						:components
						((:file "db")
						 (:file "main"
								:depends-on ("db")))))
  :description ""
  :build-operation "program-op" ;; leave as is
  :build-pathname "cl-demo-crud-app.bin"
  :entry-point "cl-demo-crud-app:main"	
  :in-order-to ((test-op (test-op "first-app/tests"))))

(defsystem "cl-demo-crud-app/tests"
	:author "pvik"
	:license "MIT"
	:depends-on ("first-app"
				 "rove")
	:components ((:module "tests"
						  :components
						  ((:file "main"))))
	:description "Test system for cl-demo-crud-app"
	:perform (test-op (op c) (symbol-call :rove :run c)))
