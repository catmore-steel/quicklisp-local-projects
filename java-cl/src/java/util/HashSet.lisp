(in-package #:java-util)

(defclass HashSet()
  ((hSet :initform (list))))

(defmethod add ((hashSet HashSet) e)
  (format t "HashSet---add---call")
  (pushnew e (slot-value hashSet 'hSet)))

(defmethod addAll ((hashSet HashSet) c)
  (dolist (e c)
    (add hashSet e)))
