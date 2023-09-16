(in-package :rd-cost.web)

(defclass SysRoleServiceImpl (ISysRoleService)
  ())

(defmethod selectRolePermissionByUserId ((sysRoleServiceImpl SysRoleServiceImpl) userId)
  (let ((perms (selectRolePermissionByUserId *SysRoleMapper* userId))
	(permsSet (hash-set:make-hash-set)))
    (dotimes (perm perms)
      (if (not (null perm))
	  (hash-set:hs-union permsSet (cl-ppcre:split #\, (slot-value perm 'roleKey)))
	  ()))
    permsSet))


