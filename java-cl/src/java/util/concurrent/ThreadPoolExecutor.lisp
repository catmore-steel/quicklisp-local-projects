(in-package #:java-util-concurrent)

(defclass ThreadPoolExecutor()
  ((corePoolSize :initarg :corePoolSize :initform 0)
   ;(maximumPoolSize :initform 0)
   ))

; error
;(defmethod initialize-instance :before ((executor ThreadPoolExecutor) &key)
;  (if (< 0 (slot-value executor 'corePoolSize))
;      (error "must > 0")
;      ()))

(defmethod initialize-instance :after ((executor ThreadPoolExecutor) &key)
  (format t "~A" (slot-value executor 'corePoolSize))
  (if (> 0 (slot-value executor 'corePoolSize))
      (format t "<0")
      (format t ">0"))
  )


