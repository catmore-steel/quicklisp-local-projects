(in-package #:java.util)

(defclass Objects ()
  ())

(defmethod requireNonNull ((objects Objects) obj)
  (return obj))
