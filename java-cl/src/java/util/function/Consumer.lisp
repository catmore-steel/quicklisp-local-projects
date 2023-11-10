(in-package #:java.util.function)

(defclass Consumer ()
  ())

(defmethod accept ((consumer Consumer) tt)
  ())

(defmethod andThen ((consumer Consumer) (after Consumer))
  ())
