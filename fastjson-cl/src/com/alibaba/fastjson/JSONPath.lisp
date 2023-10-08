(in-package #:com.alibaba.fastjson)

(defclass JSONPath ()
  ())

(defmethod eq-jsonpath ((jsonpath JSONPath) a b)
  (eq a b))






(defclass JSONPathParser ()
  ((path)
  (pos)
  (ch)
  (level)
  (hasRefSegment)
  (strArrayRegex :initform "\'\\s*,\\s*\'")
  (strArrayPatternx)))


(defmethod initialize-instance :after ((jsonpathparser JSONPathParser) &key)
  (when (slot-value jsonpathparser 'path)
    (next jsonpathparser)))

(defmethod next ((jsonpathparser JSONPathParser))
  (setf (slot-value jsonpathparser 'ch) (char (slot-value jsonpathparser 'path) (1+ (slot-value jsonpathparser 'pos)))))




(defmethod isDigitFirst ((jsonpathparser JSONPathParser) ch)
  (or (eql ch #\-) (eql ch #\+) (and (char>= ch #\0) (char<= ch #\9))))
