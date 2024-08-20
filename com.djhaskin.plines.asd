(defsystem "com.djhaskin.plines"
  :version "0.5.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
               "alexandria"
               "trivial-features"
               )
  :components ((:module "src"
          :components
          ((:file "wbtrees"))))
  :description "Pennant Lines"
  :in-order-to (
                (test-op (test-op "com.djhaskin.plines/tests"))))

(defsystem "com.djhaskin.plines/tests"
  :version "0.5.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
               "com.djhaskin.plines"
               "parachute")

  :components ((:module "test"
                :components
                ((:file "wbtrees"))))
  :description "Test system for Pennant Lines"
  :perform (asdf:test-op (op c)

                         (uiop:symbol-call
                           :parachute
                           :test :com.djhaskin.nrdl/tests)))
