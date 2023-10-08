;;;; my-utils-cl.lisp

(in-package #:my-utils-cl)

(defun get-slot-definition (class-symbol json-key)
  "returns a slot-definition from the slot of class-symbol which is infered from using json-key / method symbol"
  (find
   (cl-change-case:param-case json-key)
   (closer-mop:class-direct-slots (find-class class-symbol))
   :test (lambda(a b) (string-equal a (symbol-name (closer-mop:slot-definition-name b))))))
