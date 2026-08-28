(asdf:defsystem #:ls-flock
  :description "Process-shared advisory file locks and exclusive leases."
  :author "Lambda Symbolics OÜ"
  :license "COLL-Attribution"
  :version "0.1.0"
  :serial t
  :depends-on (#:bordeaux-threads
               #:osicat)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "flock"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:ls-flock/tests))))

(asdf:defsystem #:ls-flock/tests
  :description "Tests for ls-flock."
  :depends-on (#:ls-flock)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "tests"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:ls-flock/tests '#:run-tests)))
