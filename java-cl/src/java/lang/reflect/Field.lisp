(in-package #:java.lang.reflect)

(defclass Field ()
  ((name)))


(defmethod getName ((field Field) obj)
  (slot-value obj 'name))
