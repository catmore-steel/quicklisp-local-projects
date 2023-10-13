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
  :depends-on ("closer-mop" "cl-change-case" "my-utils-java-lang"))


(asdf:defsystem #:my-utils-java-lang
  :pathname "src/java/lang"
  :components ((:file "package")
	       (:file "in-package")
	       (:file "ClassUtils")))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            my-utils-cl-test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:defsystem #:my-utils-cl-test
  :pathname "t"
  :components ((:file "package")
	       (:file "in-package"))
  :depends-on ("fiveam" "my-utils-cl" "test-java-lang"))

(asdf:defsystem #:test-java-lang
  :pathname "t/java/lang"
  :components ((:file "package")
	       (:file "in-package")
	       (:file "ClassUtilsTest")))

