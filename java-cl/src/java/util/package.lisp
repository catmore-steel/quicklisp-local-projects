



;;;; package.lisp

(defpackage #:java-util
  (:use #:cl)
  (:export #:Properties
	   #:propMap;CL-USER> (slot-value (make-instance 'java-util:Properties) 'java-util:propMap)//在CL包中的访问方式
	   #:getProperty
	   
	   #:HashSet
	   #:HashSet.add;;TODO compile error and can't fix it
	   #:add
	   #:addAll))
