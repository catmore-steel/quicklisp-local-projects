(in-package :rd-cost.web)

(defclass SysPermissionService()
  ())

(defmethod getMenuPermission ((sysPermissionService SysPermissionService) (user SysUser))
  (let ((hSet (make-instance 'java-util:HashSet)))
    (if (isAdmin user)
	(java-util:add hSet "*:*:*")
	(java-util:addAll hset (selectMenuPermsByUserId (make-instance 'SysMenuServiceImpl) (slot-value user 'user-id))))
    hSet))
