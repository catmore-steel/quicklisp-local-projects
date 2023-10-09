(in-package #:java.util)

(defclass Spliterators ()
  ())


(defmethod spliterator ((spliterator Spliterators) array fromIndex toIndex additionalCharacteristics)
  ;; TODO checkFromToBounds
  (make-instance-ArraySpliterator spliterator array fromIndex toIndex additionalCharacteristics))


(defmethod make-instance-ArraySpliterator ((spliterator Spliterators) array origin fence additionalCharacteristics)
  (make-instance 'ArraySpliterator :array array :index origin :fence fence :characteristics additionalCharacteristics))
