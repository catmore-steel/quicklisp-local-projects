(in-package :rd-cost.web)

(defclass UserDetailsServiceImpl()
  ())


(defmethod loadUserByUsername((userDetailsServiceImpl UserDetailsServiceImpl) username)
  (createLoginUser (mito:find-dao 'SysUser :user-name username)))


(defmethod createLoginUser((user SysUser))
  (make-instance 'LoginUser :user user :permissions (getMenuPermission (make-instance 'SysPermissionService) user)))
