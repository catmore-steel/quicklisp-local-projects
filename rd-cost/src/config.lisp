(in-package :cl-user)
(defpackage rd-cost.config
  (:use :cl)
  (:import-from :envy
                :config-env-var
                :defconfig)
  (:export :config
           :*application-root*
           :*static-directory*
           :*template-directory*
           :appenv
           :developmentp
           :productionp))
(in-package :rd-cost.config)

(setf (config-env-var) "APP_ENV")

(defparameter *application-root*   (asdf:system-source-directory :rd-cost))
(defparameter *static-directory*   (merge-pathnames #P"static/" *application-root*))
(defparameter *template-directory* (merge-pathnames #P"templates/" *application-root*))

;;(defconfig :common
;;  `(:databases ((:maindb :mysql :database-name "jq_rd_cost" :username "root" :password "123456" :host "192.168.1.20" :port 3309))))

(defconfig :common
  `(:databases ((mito:connect-toplevel :mysql :database-name "jq_rd_cost_vm" :username "root" :password "root" :host "127.0.0.1" :port 3306))))

(defconfig |development|
  '())

(defconfig |production|
  '())

(defconfig |test|
  '())

(defun config (&optional key)
  (envy:config #.(package-name *package*) key))

(defun appenv ()
  (uiop:getenv (config-env-var #.(package-name *package*))))

(defun developmentp ()
  (string= (appenv) "development"))

(defun productionp ()
  (string= (appenv) "production"))
