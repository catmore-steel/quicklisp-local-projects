(in-package #:java.util.stream)

(defclass StreamSupport ()
  ())

(defmethod stream ((streamsupport StreamSupport) (spliterator Spliterator) parallel)
  (make-instance 'Head :source spliterator :sourceFlags (fromCharacteristics (make-instance 'StreamOpFlag spliterator)) :parallel parallel))
