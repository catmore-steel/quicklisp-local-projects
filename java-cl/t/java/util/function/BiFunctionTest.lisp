(in-package #:test.java.util.function)

;; https://www.geeksforgeeks.org/java-bifunction-interface-methods-apply-and-addthen/



(test Main1
  (is (= 5 (jApply (make-instance 'BiFunction :lmbd (lambda(a b)(+ a b))) 2 3))))



(test my-test
  (is (= 2 (+ 1 1))))


