;;;; fastjson-cl-test.asd

(asdf:defsystem #:fastjson-cl-test
  :description "Describe fastjson-cl-test here"
  :author "catmore"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :pathname "t"
  :components ((:file "package")
               (:file "fastjson-cl"))
  :depends-on ("com-alibaba-fastjson-test"))

(asdf:defsystem #:com-alibaba-fastjson-test
  :serial t
  :pathname "t/com/alibaba/fastjson"
  :components ((:file "package")
               (:file "com-alibaba-fastjson")
               (:file "JSONPathTest")))
