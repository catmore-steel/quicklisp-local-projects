
(ql:quickload :rd-cost)

(defpackage rd-cost.app
  (:use :cl)
  (:import-from :lack.builder
   :builder)
  (:import-from :lack.session.store.redis)
  (:import-from :java-util)
  (:import-from :useragentutils-cl)
  (:import-from :ppcre
                :scan
                :regex-replace)
  (:import-from :rd-cost.web
                :*web*)
  (:import-from :rd-cost.config
                :config
                :productionp
                :*static-directory*))
(in-package :rd-cost.app)

(builder
 (:static
  :path (lambda (path)
          (if (ppcre:scan "^(?:/images/|/css/|/js/|/robot\\.txt$|/favicon\\.ico$)" path)
              path
              nil))
  :root *static-directory*)
 (if (productionp)
     nil
     :accesslog)
 (if (getf (config) :error-log)
     `(:backtrace
       :output ,(getf (config) :error-log))
     nil)
 :session
 (if (productionp)
     nil
     (lambda (app)
       (lambda (env)
         (funcall app env))))
 *web*)
