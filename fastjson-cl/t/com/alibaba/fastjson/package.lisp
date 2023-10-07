(defpackage #:test.com.alibaba.fastjson
  (:use #:cl #:fiveam)
  (:import-from :com.diffblue.deeptestutils
		:forName)
  (:import-from :com.alibaba.fastjson
   :JSONPath
   :eq-jsonpath))
