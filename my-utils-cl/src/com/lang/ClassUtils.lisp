(in-package #:com.lang)

(defgeneric getDeclaredFields (class-symbol)
  (closer-mop:class-direct-slots (find-class class-symbol)))
