(in-package #:java.util.function)

(defclass JFunction ()
  ())

(defmethod jApply ((jFunction JFunction) tt)
  ())

(defmethod compose ((jFunction JFunction) (before JFunction))
  (lambda (v) (apply jFunction (apply before v))))

(defmethod andThen ((jFunction JFunction) (after JFunction))
  (lambda (tt) (apply after (apply jFunction tt))))

(defmethod jIdentity ((jFunction JFunction))
  (lambda ()()))
