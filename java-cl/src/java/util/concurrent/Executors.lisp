(in-package #:java-util-concurrent)

(defclass Executors()
  ())


(defmethod newFixedThreadPool((executors Executors) nThreads)
  (let ((inst (make-instance 'ThreadPoolExecutor :corePoolSize nThreads)))
    inst)
  )
