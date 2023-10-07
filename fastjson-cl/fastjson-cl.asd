;;;; fastjson-cl.asd

(asdf:defsystem #:fastjson-cl
  :description "Describe fastjson-cl here"
  :author "catmore"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "fastjson-cl"))
  :depends-on ("abstract-classes" "interface" "defenum" "deeptestutils-cl"  "com.alibaba.fastjson"))


(asdf:defsystem #:com.alibaba.fastjson
  :serial t
  :pathname "src/com/alibaba/fastjson"
  :components ((:file "package")
               (:file "com-alibaba-fastjson")
               (:file "JSON")
               (:file "JSONArray")
               (:file "JSONAware")
               (:file "JSONException")
               (:file "JSONObject")
               (:file "JSONPatch")
               (:file "JSONPath")
               (:file "JSONPathException")
               (:file "JSONPObject")
               (:file "JSONReader")
               (:file "JSONStreamAware")
               (:file "JSONStreamContext")
               (:file "JSONValidator")
               (:file "JSONWriter")
               (:file "PropertyNamingStrategy")
               (:file "TypeReference")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; fastjson-cl-test.asd
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:defsystem #:fastjson-cl-test
  :description "Describe fastjson-cl-test here"
  :author "catmore"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :pathname "t"
  :components ((:file "package")
               (:file "fastjson-cl-test"))
  :depends-on ("fiveam" "test.com.alibaba.fastjson"))

(asdf:defsystem #:test.com.alibaba.fastjson
  :serial t
  :pathname "t/com/alibaba/fastjson"
  :components ((:file "package")
               (:file "com-alibaba-fastjson")
               (:file "JSONPathTest")))








