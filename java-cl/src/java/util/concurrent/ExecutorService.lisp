(in-package #:java-cl)

(defclass ExecutorService(Executor)
  ())


(defmethod submit((callable Callable))
  ())
