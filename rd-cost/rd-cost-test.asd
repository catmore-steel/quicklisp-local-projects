(defsystem "rd-cost-test"
  :defsystem-depends-on ("prove-asdf")
  :author "catmore"
  :license ""
  :depends-on ("rd-cost"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "rd-cost"))))
  :description "Test system for rd-cost"
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
