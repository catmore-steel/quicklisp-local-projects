;;;; useragentutils-cl.asd

(asdf:defsystem #:useragentutils-cl
  :description "Describe useragentutils-cl here"
  :author "catmore"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on ("defenum")
  :components ((:file "package")
               (:file "useragentutils-cl")
	       (:file "src/UserAgent")
	       ;(:file "src/Browser" :depends-on ("src/Manufacturer"))
	       (:file "src/Manufacturer")
	       (:file "src/BrowserType")
	       (:file "src/RenderingEngine")
	       (:file "src/Browser")))
