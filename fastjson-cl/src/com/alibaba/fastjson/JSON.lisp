(in-package :com.alibaba.fastjson)

;; (use-package :org.tfeb.hax.abstract-classes)

(defclass JSON ()
  ())

(defmethod json-static ((json JSON))
  ())


(defmethod parse ((json JSON) text)
  ())

(defmethod parseObject ((json JSON) text)
  (let ((obj (parse json text)))
    ()))
