(in-package #:java.lang.reflect)

(defclass Field ()
  ((name)
   (sdsd :initarg :sdsd) ;; STANDARD-DIRECT-SLOT-DEFINITION
   ))


(defmethod getName ((field Field))
  (closer-mop:slot-definition-name (slot-value field 'sdsd)))


(defmethod set ((field Field) obj value)
  ())
