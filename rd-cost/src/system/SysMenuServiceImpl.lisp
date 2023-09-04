(in-package :rd-cost.web)

(defclass SysMenuServiceImpl (ISysMenuService)
  ())

(defmethod selectMenuPermsByUserId ((sysMenuServiceImpl SysMenuServiceImpl) userId)
  (let ((perms (selectMenuPermsByUserId (make-instance 'SysMenuMapper) userId))
	(hSet (make-instance 'java-util:HashSet)))
    (dolist (perm perms)
      (if (not (null perm))
	  (java-util:addAll hSet (cl-ppcre:split "," perm))))
    hSet))
