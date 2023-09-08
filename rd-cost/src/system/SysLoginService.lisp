(in-package :rd-cost.web)

(defclass SysLoginService()
  ())

(defmethod login((sysLoginService SysLoginService) username password code uuid)
  (let* ((loginUser (loadUserByUsername *UserDetailsServiceImpl* username))
	 (userIdKey (concatenate 'string "jq-rd-cost:" "login_userid:" (write-to-string (slot-value (slot-value loginUser 'user) 'user-id))))
	 (userKey (getCacheObject *RedisCache* userIdKey)))
    (if (not (null userKey)) 
	(progn
	  (deleteObject *RedisCache* userIdKey)
	  (deleteObject *RedisCache* userKey)
	  ))
    (createToken *TokenService* loginUser)))
