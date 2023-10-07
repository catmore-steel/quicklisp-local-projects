(in-package test.com.alibaba.fastjson)

(use-package #:com.alibba.fastjson)

(test eq1
  (let ((a -1)
        (b nil)
        (c (forName (make-instance 'Reflector) 'com.alibaba.fastjson:JSONPath)))
    ()))
