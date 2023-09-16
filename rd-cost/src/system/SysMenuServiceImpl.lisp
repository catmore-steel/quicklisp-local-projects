(in-package :rd-cost.web)

(defclass SysMenuServiceImpl (ISysMenuService)
  ())

(defmethod selectMenuPermsByUserId ((sysMenuServiceImpl SysMenuServiceImpl) userId)
  (let ((perms (selectMenuPermsByUserId *SysMenuMapper* userId))
	(permsSet (hash-set:make-hash-set)))
    (dolist (perm perms)
      (if (not (null perm))
	  (hash-set:hs-union permsSet (cl-ppcre:split "," perm))))
    permsSet))
