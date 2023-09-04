(in-package :rd-cost.web)

(defclass SysLoginService()
  ())

(defmethod login((sysLoginService SysLoginService) username password code uuid)
  (let ((loginUser (loadUserByUsername (make-instance 'UserDetailsServiceImpl) username)))
    (createToken (make-instance 'TokenService) loginUser)))

