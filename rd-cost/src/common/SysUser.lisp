(in-package :rd-cost.web)

(defclass SysUser ()
  ((user-id :col-type (:bigint 0) :accessor userId)
   (dept-id :col-type (:bigint 0) :accessor deptId)
   (user-name :col-type (:varchar 30) :accessor userName)
   (email :col-type (or (:varchar 128) :null) :accessor email)
   (phonenumber :col-type (:varchar 20) :accessor phonenumber))
  (:metaclass mito:dao-table-class)
  (:table-name "sys_user"))

(defmethod isAdmin ((sysUser SysUser))
  (and (not (null (slot-value sysUser 'user-id)))
       (eql 1 (slot-value sysUser 'user-id))))

