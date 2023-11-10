(in-package #:java.util)

(defclass Spliterator ()
  ((SORTED :initform #x00000004)))



(defmethod tryAdvance ((spliterator Spliterator) )
  ())

(defmethod characteristics ((spliterator Spliterator))
  ())

(defmethod getComparator ((spliterator Spliterator))
  ())



(defclass Type ()
  ())















;; MaskBuilder

(defclass MaskBuilder ()
  ())
