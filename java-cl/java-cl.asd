;;;; java-cl.asd

(asdf:defsystem #:java-cl
  :description "Describe java-cl here"
  :author "catmore"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "java-cl"))
  :depends-on ("closer-mop" "cl-change-case" "java-lang"))

(asdf:defsystem #:java-io
  :pathname "src/java/io"
  :components ((:file "package")
	       (:file "java-io")
	       (:file "Reader")))


(asdf:defsystem #:java-lang
  :pathname "src/java/lang"
  :components ((:file "package")
	       (:file "in-package")
	       (:file "Class")))


(asdf:defsystem #:java-lang-reflect
  :pathname "src/java/lang/reflect"
  :components ((:file "package")
	       (:file "in-package")
	       (:file "Field")))



(asdf:defsystem #:java-util
  :description "Describe java-cl here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :pathname "src/java/util"
  :components ((:file "package")
               (:file "java-util")
	       (:file "Properties")
	       (:file "HashSet")
	       (:file "Arrays")
	       (:file "Spliterator")
	       (:file "Spliterators")))


(asdf:defsystem #:java-util-concurrent
  :pathname "src/java/util/concurrent"
  :components ((:file "package")
	       (:file "java-util-concurrent")
	       (:file "AbstractExecutorService"
		      :depends-on ("Callable" "Executor"))
	       (:file "Callable")
	       (:file "Future")
	       (:file "FutureTask")
	       (:file "Executor"
		      :depends-on ("Runnable"))
	       (:file "Executors")
	       (:file "Runnable")
	       (:file "RunnableFuture")
	       (:file "ThreadPoolExecutor")))


(asdf:defsystem #:java-util-function
  :pathname "src/java/util/function"
  :components ((:file "package")
	       (:file "in-package")
	       (:file "BiFunction"
		      :depends-on ("Function"))
	       (:file "Consumer")
	       (:file "Function")))



(asdf:defsystem #:java-util-stream
  :pathname "src/java/util/stream"
  :components ((:file "package")
	       (:file "in-package")
	       (:file "Stream")))


(asdf:defsystem #:java-sql
  :pathname "src/java/sql"
  :components ((:file "package")
	       (:file "java-sql")
	       (:file "Connection")))


(asdf:defsystem #:javax-sql
  :pathname "src/javax/sql"
  :components ((:file "package")
	       (:file "javax-sql")
	       (:file "DataSource")))




;;;;;;;;;;;;;;;;;;;;;;;;;
;;      unit-test
;;;;;;;;;;;;;;;;;;;;;;;;;


(asdf:defsystem #:java-lang-test
  :pathname "t/java/lang"
  :components ((:file "package")
	       (:file "in-package")
	       (:file "ClassTest"))
  :depends-on ("fiveam" "java-lang"))


(asdf:defsystem #:java-util-function-test
  :pathname "t/java/util/function"
  :components ((:file "package")
	       (:file "in-package")
	       (:file "BiFunctionTest"))
  :depends-on ("fiveam" "java-util-function"))
