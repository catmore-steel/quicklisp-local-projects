(in-package #:java.util.stream)

(defclass StreamOpFlag ()
  ())

(defmethod fromCharacteristics ((streamopflag StreamOpFlag) (spliterator Spliterator))
  (let ((characteristics (characteristics spliterator )))
    (if ()
	()
	())))
