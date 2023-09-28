;;;; fastjson-cl.asd

(asdf:defsystem #:fastjson-cl
  :description "Describe fastjson-cl here"
  :author "catmore"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "fastjson-cl"))
  :depends-on ("abstract-classes" "interface" "defenum"))
