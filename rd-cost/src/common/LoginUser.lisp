(in-package :rd-cost.web)

(defclass LoginUser ()
  ((token :initform nil)
   (loginTime :initform nil)
   (expireTime :initform nil)
   (ipaddr :initform nil)
   (loginLocation :initform nil)
   (browser :initform nil)
   (os :initform nil)
   (user :initarg :user)
   (permissions :initarg :permissions)))
