;;;; deeptestutils-cl.asd

(asdf:defsystem #:deeptestutils-cl
  :description "Describe deeptestutils-cl here"
  :author "catmore"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "deeptestutils-cl"))
  :depends-on ("com.diffblue.deeptestutils"))

(asdf:defsystem #:com.diffblue.deeptestutils
  :pathname "src/com/diffblue/deeptestutils"
  :components ((:file "package")
	       (:file "com-diffblue-deeptestutils")
	       (:file "Reflector")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      deeptestutils-cl-test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:defsystem #:deeptestutils-cl-test
  :pathname "t"
  :components ((:file "package")
	       (:file "deeptestutils-cl-test")))

(asdf:defsystem #:test.com.diffblue.deeptestutils
  :pathname "t/com/diffblue/deeptestutils"
  :components ((:file "package")
	       (:file "com-diffblue-deeptestutils")))

