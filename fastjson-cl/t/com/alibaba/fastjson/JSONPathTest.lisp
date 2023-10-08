(in-package #:test.com.alibaba.fastjson)

(test eq1
  (let* ((a -1)
        (b nil)
        (c (forName (make-instance 'Reflector) 'JSONPath))
	(m (ensure-generic-function 'eq-jsonpath))
	(retval (funcall m (make-instance 'JSONPath) a b))
	)
    (is (eql nil retval))))

(test eq2
  (let* ((a nil)
	(b nil)
	(c (forName (make-instance 'Reflector) 'JSONPath))
	(m (ensure-generic-function 'eq-jsonpath))
	(retval (funcall m (make-instance 'JSONPath) a b))
	)
    (is (eql T retval))))

(test isDigitFirst1
  (let* ((ch #\2)
	 (m (ensure-generic-function 'isDigitFirst))
	 (retval (funcall m (make-instance 'JSONPathParser) ch))
	 )
    (is (eql T retval))))

(test isDigitFirst2
  (let* ((ch #\:)
	 (m (ensure-generic-function 'isDigitFirst))
	 (retval (funcall m (make-instance 'JSONPathParser) ch))
	 )
    (format t "retval=~A~%" retval)
    (is (eql nil retval))))

(test isDigitFirst3
  (let* ((ch #\u0000)
	 (m (ensure-generic-function 'isDigitFirst))
	 (retval (funcall m (make-instance 'JSONPathParser) ch))
	 )
    (is (eql nil retval))))

(test isEOF1
  (let* ((objectUnderTest (getInstance (make-instance 'Reflector) 'JSONPathParser))
	 )
    (setField (make-instance 'Reflector) "path" "")
    (setField (make-instance 'Reflector) "pos" -2147483647)
    (setField (make-instance 'Reflector) "level" 0)
    (setField (make-instance 'Reflector) "ch" '\u0000')
    (let* ((c (forName (make-instance 'Reflector) 'JSONPathParser))
	   (m (ensure-generic-function 'isEOF))
	   (retval (funcall m (make-instance 'JSONPathParser) objectUnderTest)))
      (is (eql nil retval)))))

(test my-test
  (is (= 2 (+ 1 1))))
