(in-package #:java.util)

(defclass Spliterators ()
  ())


(defmethod spliterator ((spliterator Spliterators) array fromIndex toIndex additionalCharacteristics)
  ;; TODO checkFromToBounds
  (make-instance 'ArraySpliterator :array array :index fromIndex :fence toIndex :characteristics (let* ((instance (make-instance 'Spliterator))
													(SIZED (slot-value instance 'SIZED))
													(SUBSIZED (slot-value instance 'SUBSIZED)))
												   (logior SIZED SUBSIZED))))
(defmethod characteristics ((spliterator Spliterator))
  ())

(defmethod hasCharacteristics ((spliterator Spliterator) characteristics)
  (= (logand (characteristics spliterator) characteristics) characteristics))




;;;;;;;;  ArraySpliterator  ;;;;;;;;

(defclass ArraySpliterator (Spliterator)
  ((array :initargs)
   (index :initargs)
   (fence :initargs)
   (characteristics :initargs)))


(defmethod getComparator ((arraySpliterator ArraySpliterator))
  (if (hasCharacteristics)
      (return nil)))  
  
  
(defmethod characteristics ((arraySpliterator ArraySpliterator))
  (slot-value arraySpliterator 'characteristics))
