(defpackage #:test.com.alibaba.fastjson
  (:use #:cl #:fiveam)
  (:import-from :com.diffblue.deeptestutils
		#:Reflector
		#:forName
		#:getInstance)
  (:import-from :com.alibaba.fastjson
		#:JSONPath
		#:eq-jsonpath
		
		#:JSONPathParser
		#:isDigitFirst))
