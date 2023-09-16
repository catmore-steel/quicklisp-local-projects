(in-package :rd-cost.web)

(defclass SysPermissionService()
  ())

(defmethod getMenuPermission ((sysPermissionService SysPermissionService) (user SysUser))
  (let ((perms (hash-set:make-hash-set)))
    (if (isAdmin user)
	(hash-set:hs-insert perms "*:*:*")
	(hash-set:hs-union perms (selectMenuPermsByUserId *ISysMenuService* (slot-value user 'user-id))))
    perms))


(defmethod getRolePermission ((sysPermissionService SysPermissionService) (user SysUser))
  (let ((roles (hash-set:make-hash-set)))
    (if (isAdmin user)
	(hash-set:hs-insert roles "admin")
	(hash-set:hs-union roles ()))))
