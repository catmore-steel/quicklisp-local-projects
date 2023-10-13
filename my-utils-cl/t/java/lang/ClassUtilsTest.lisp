(in-package #:test.my.utils.java.lang)

(defclass Foo ()
  ())
(defmethod initialize-instance ()
  )

(defmethod foo-gf ((foo Foo))
  ())

(defmethod foo-gf2 ((foo Foo))
  ())


(test getDeclaredMethod
  (is (eql 'foo-gf (my.utils.java.lang:getDeclaredMethod 'Foo "foo-gf" nil))))

(test forName
  (is (eql (c2mop:classp (my.utils.java.lang:forName 'Foo))) T))

(test my-test
  (is (= 2 (+ 1 1))))

