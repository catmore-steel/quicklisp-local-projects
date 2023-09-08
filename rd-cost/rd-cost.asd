(defsystem "rd-cost"
  :version "0.1.0"
  :author "catmore"
  :license ""
  :depends-on ("clack"
               "lack"
	       "lack-session-store-redis"
               "caveman2"
               "envy"
               "cl-ppcre"
               "uiop"

               ;; for @route annotation
               "cl-syntax-annot"

               ;; HTML Template
               "djula"

               ;; for DB
               "datafly" ;; can't use defclass just defmodel or defstruct, see doc
	       "mito"
               "sxql"
	       ;;"cl-json" ;; cant't out json data to response ,datafly can
	       ;; for token
	       "jose"
	       ;; my custom lib
	       "java-util"
	       "useragentutils-cl"
	       
	       "cl-ppcre"
	       "uuid"
	       "defenum")
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "config")
		 (:file "common/LoginBody")
		 (:file "common/LoginUser")
		 (:file "common/SysUser")
		 (:file "common/AjaxResult")
		 (:file "common/Constants")
		 (:file "framework/SysLoginService")
		 (:file "framework/TokenService")
		 (:file "framework/UserDetailsServiceImpl")
		 (:file "framework/SysPermissionService")
		 (:file "system/SysLoginController")
		 (:file "system/SysLoginService")
		 (:file "system/ISysMenuService")
		 (:file "system/SysMenuMapper"))))
  :description ""
  :in-order-to ((test-op (test-op "rd-cost-test"))))
