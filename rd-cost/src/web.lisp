(in-package :cl-user)
(defpackage rd-cost.web
  (:use :cl
        :caveman2
        :rd-cost.config
        :rd-cost.view
        :rd-cost.db
        :datafly
        :mito
        :sxql
	;;:cl-json
	:jose)
  (:export :*web*))
(in-package :rd-cost.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html"))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))


;;
;; db

(mito:connect-toplevel :mysql :database-name "jq_rd_cost_vm" :username "root" :password "root" :host "127.0.0.1" :port 3306)

;;
;; redis

;;(defvar *redis* (lack.session.store.redis:make-redis-store :host "192.168.1.20" :port 6379))
(defvar *RedisCache* (make-instance 'RedisCache)) ;; Service or @Component

;;
;; Controller



;;
;; Service
(defvar *SysLoginService*  (make-instance 'SysLoginService))
(defvar *TokenService* (make-instance 'TokenService))


;;
;; Service Impl
(defvar *UserDetailsServiceImpl* (make-instance 'UserDetailsServiceImpl))

;;
;; Other 

(defvar *AjaxResult* (make-instance 'AjaxResult))
(defvar *Constants* (make-instance 'Constants))

;;
;; jose

(defvar *my$ecret* (ironclad:ascii-string-to-byte-array "my$ecret"))

