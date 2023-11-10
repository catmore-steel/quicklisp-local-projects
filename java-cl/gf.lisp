(defpackage #:java-gf
  (:use #:cl #:java.util.function)
  (:import-from #:java.util.function
		#:JFunction
		#:BiFunction))

(in-package #:java-gf)

(defgeneric jApply (class &optional tt)
  (:documentation  "xxx")
  (:method ((class JFunction) &optional tt))
  (:method ((class BIFunction) &optional tt v)))
