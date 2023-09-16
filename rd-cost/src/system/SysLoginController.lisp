
;; system--SysLoginController
(in-package :rd-cost.web)

; ok
;(defroute "/login" ()
;  (format t "login"))

; ok
;(defroute "/login" ()
;  (render-json '(:a 1 :b 2)))

; ok
;(defroute ("/login" :method :POST) (&key |username| |password|)
;  (render-json (login (make-instance 'SysLoginService) |username| |password| nil nil)))

; ok
;(defroute ("/login" :method :POST) (&key |username| |password|)
;  (render-json (make-instance 'AjaxResult :code 200 :msg "ok"
;					  :token (jose:encode :hs256 (ironclad:ascii-string-to-byte-array "my$ecret")
;							      (datafly:convert-object (login (make-instance 'SysLoginService) |username| |password| nil nil))))))

(defroute ("/login" :method :POST) (&key |username| |password|)
  (let ((ajax (success *AjaxResult*))
	(token (login *SysLoginService* |username| |password| nil nil)))
    (setf (gethash 'token ajax) token)
    (render-json ajax)))


(defroute "/getInfo" ()
  (let* ((loginUser (getLoginUser *TokenService*))
	 (user (slot-value loginUser 'user))
	 (roles (getRolePermission *SysPermissionService* user))
	 (permissions (getMenuPermission *SysPermissionService* user))
	 (ajax (success *AjaxResult*))
	 )
    (setf (gethash 'user ajax) user)
    (setf (gethash 'roles ajax) roles)
    (setf (gethash 'permissions ajax) permissions)
    ajax))

