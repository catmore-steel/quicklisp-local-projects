(in-package #:test.com.alibaba.fastjson)

(test eq1
  (let* ((a -1)
        (b nil)
        (c (forName (make-instance 'Reflector) 'JSONPath))
	(m (ensure-generic-function 'eq-jsonpath))
	(retval (funcall m (make-instance 'JSONPath) a b))
	)
    (is (null retval)))
  )

(test eq2
  (let* ((a nil)
	(b nil)
	(c (forName (make-instance 'Reflector) 'JSONPath))
	(m (ensure-generic-function 'eq-jsonpath))
	(retval (funcall m (make-instance 'JSONPath) a b))
	)
    (is (null retval))))

(test isDigitFirst1
  (let* ((ch "2")
	 )
    ()))


(test my-test
  (is (= 2 (+ 1 1))))
