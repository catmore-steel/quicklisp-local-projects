(in-package #:com.alibaba.fastjson)

(defclass JSONPath ()
  ())

(defmethod eq-jsonpath ((jsonpath JSONPath) a b)
  (eq a b))
