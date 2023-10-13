(in-package #:java.lang)

(defclass JClass ()
  ())

(defmethod getDeclaredMethod ((jclass JClass) class-symbol name &rest parameterTypes)
  (find 
   (cl-change-case:param-case name)
   (c2mop:specializer-direct-methods (find-class class-symbol))
   :test (lambda(a b) (let* ((mgf (c2mop:method-generic-function b))
			     (gfn (c2mop:generic-function-name mgf))
			     (sn (symbol-name gfn)))
			(format t "b=~A,mgf=~A,gfn=~A,sn=~A" b mgf gfn sn)
			(string-equal a sn)))))



(defmethod getDeclaredFields (class)
  (c2mop:class-direct-slots class))

(defmethod getDeclaredField (class name)
  (find
   (cl-change-case:param-case name)
   (closer-mop:class-direct-slots class)
   :test (lambda(a b) (string-equal a (symbol-name (closer-mop:slot-definition-name b))))))
