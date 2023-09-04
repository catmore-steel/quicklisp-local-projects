(in-package #:java-util-concurrent)

(defclass AbstractExecutorService (ExecutorService)
  ())

(defmethod submit((task Callable))
  (let ((ftask (newTaskFor task)))
    (execute ftask)))


(defmethod newTaskFor((callable Callable))
  (let ((futureTask (make-instance 'FutureTask :callable callable)))
    futureTask))

(defmethod execute((executor Executor))
  ())
