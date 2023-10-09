(in-package #:java.util)

(defclass Arrays ()
  ())

(defmethod stream ((array Arrays) array)
  (stream3 array 0 (array-total-size array)))


(defmethod stream3 ((array Arrays) startInclusive endExclusive)
  ())

(defmethod spliterator ((array Arrays) startInclusive endExclusive)
  ())

