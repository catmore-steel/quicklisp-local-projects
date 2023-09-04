(in-package :rd-cost.web)

(defclass SysMenuMapper ()
  ())


(defmethod selectMenuPermsByUserId ((sysMenuMapper SysMenuMapper) userId)
  (mito:retrieve-by-sql (concatenate 'string
       "select distinct sys_menu.perms
        from sys_menu
        left join sys_role_menu on sys_menu.menu_id = sys_role_menu.menu_id
        left join sys_user_role on sys_role_menu.role_id = sys_user_role.role_id
        left join sys_role on sys_role.role_id = sys_user_role.role_id
        where sys_menu.`status` = '0' and sys_role.`status` = '0' and sys_user_role.user_id =" userId)))
