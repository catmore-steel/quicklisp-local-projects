(in-package #:java.util.stream)

(defclass StreamOpFlag ()
  ())

(defmethod createMask ((streamOpFlag StreamOpFlag) (type Type))
  ())

(defmethod fromCharacteristics ((streamopflag StreamOpFlag) (spliterator Spliterator))
  (let ((characteristics (characteristics spliterator)))
    (if (and (/= (logand characteristics (slot-value (make-instance 'Spliterator) 'SORTED)) 0)
	     (not (null (getComparator (make-instance 'Spliterator)))))
	(return (logand characteristics ))
	())))




;;;;;;;;  Type  ;;;;;;;;

(defclass Type ()
  ((SPLITERATOR)
   (STREAM)
   (OP)
   (TERMINAL_OP)
   (UPSTREAM_TERMINAL_OP)))


;;;;;;;  MaskBuilder  ;;;;;;;;

(defclass MaskBuilder ()
  ((hmap :initargs :initform (make-hash-map))))

(defmethod mask ((maskBuilder MaskBuilder) (type Type) (i Fixnum))
  ())
