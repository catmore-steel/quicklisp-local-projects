(in-package #:java-util-concurrent)

(defclass FutureTask()
  ((callable :initarg :callable)
   (state :initform 0)))

(defmethod futureTask-run((futureTask FutureTask))
  (print "FutureTask run..."))
