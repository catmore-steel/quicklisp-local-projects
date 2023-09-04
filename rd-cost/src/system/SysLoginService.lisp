(in-package :rd-cost.web)

(defclass SysLoginService()
  ())

(defmethod login((sysLoginService SysLoginService) username password code uuid)
  (let* ((loginUser (loadUserByUsername (make-instance 'UserDetailsServiceImpl) username))
	 (userIdKey (concatenate 'string "jq-rd-cost:" "login_userid:" (write-to-string (slot-value (slot-value loginUser 'user) 'user-id))))
	 (userKey (lack.session.store.redis:fetch-session *redis* userIdKey)))
    (if (not (null userKey)) 
	(progn 
	  (lack.session.store.redis:remove-session *redis* userIdKey)
	  (lack.session.store.redis:remove-session *redis* userKey)))
    (createToken (make-instance 'TokenService) loginUser)))
