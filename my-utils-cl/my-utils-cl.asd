;;;; my-utils-cl.asd

(asdf:defsystem #:my-utils-cl
  :description "Describe my-utils-cl here"
  :author "catmore"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "my-utils-cl"))
  :depends-on ("closer-mop" "cl-change-case"))


(asdf:defsystem #:langutils
  :pathname "src/com/lang"
  :components ((:file "package")
	       (:file "in-package")
	       (:file "classutils")))
