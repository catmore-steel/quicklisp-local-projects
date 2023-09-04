(in-package #:javax-sql)

(defclass DataSource ()
  ())


(defmethod getConnection((connection java-sql:Connection))
  (values connection))
