(in-package #:my.utils.java.lang)

;; ok
;;(defgeneric getDeclaredFields ((symbol class-symbol))
;;(c2mop:class-direct-slots (find-class class-symbol)))

;; ok
;; (defmethod getDeclaredMethod (class-symbol name parameterTypes)
;;   (find 
;;    (cl-change-case:param-case name)
;;    (c2mop:specializer-direct-methods (find-class class-symbol))
;;    :test (lambda(a b) (string-equal a (symbol-name (c2mop:generic-function-name (c2mop:method-generic-function b)))))))

(defmethod getDeclaredMethod (class-symbol name parameterTypes)
  (find 
   (cl-change-case:param-case name)
   (c2mop:specializer-direct-methods (find-class class-symbol))
   :test (lambda(a b) (let* ((mgf (c2mop:method-generic-function b))
			     (gfn (c2mop:generic-function-name mgf))
			     (sn (symbol-name gfn)))
			(format t "b=~A,mgf=~A,gfn=~A,sn=~A" b mgf gfn sn)
			(string-equal a sn)))))



(defgeneric forName (class))

(defmethod forName ((class symbol))
  (find-class class))

