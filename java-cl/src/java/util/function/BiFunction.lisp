(in-package #:java.util.function)

(defclass BiFunction ()
  ())

(defmethod jApply ((biFunction BiFunction) tt u)
  ())

(defmethod andThen ((biFunction BiFunction) (after JFunction))
  (lambda (tt u) (apply after (apply biFunction tt u))))

