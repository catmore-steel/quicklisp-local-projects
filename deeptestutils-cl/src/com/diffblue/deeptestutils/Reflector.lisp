(in-package #:com.diffblue.deeptestutils)

(defclass Reflector ()
  ())


;; 192
(defmethod forName ((reflector Reflector) className)
  (find-class className))

;; 391
(defmethod getInstance ((reflector Reflector) className)
  (make-instance className))



