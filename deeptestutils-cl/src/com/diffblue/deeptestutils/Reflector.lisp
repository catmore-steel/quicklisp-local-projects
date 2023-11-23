(in-package #:com.diffblue.deeptestutils)

(defclass Reflector ()
  ())

(defmethod forName ((reflector Reflector) className)
  (find-class className))

(defmethod getInstance ((reflector Reflector) className)
  (make-instance className))

;; (defgeneric setField (Reflector a b c) )

(defmethod setField ((reflector Reflector) obj fieldName newVal)
  (setField4 reflector (find-class obj) obj fieldName newVal))

(defmethod setField4 ((reflector Reflector) (c standard-class) o fieldName newVal)
  (let ((field nil)
	(declaredFields (java.lang:getDeclaredFields c)))
    (loop for field in declaredFields
	  when (eql fieldName (getName field))
	    (set field o newVal))))


