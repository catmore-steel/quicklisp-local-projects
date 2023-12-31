(in-package :rd-cost.web)

(defparameter *token* nil)

(defclass TokenService ()
  ((expireTime :initform 30)
   (soloLogin :initform nil)
   (MILLIS_MINUTE :initform (* 60 1000)))
  )

(defmethod createToken ((tokenService TokenService) (loginUser LoginUser))
  (format t "Tokenservice createtoken start ~A~%" (datafly:convert-object loginUser))
  ;(jose:encode :hs256 (ironclad:ascii-string-to-byte-array "my$ecret") (datafly:convert-object loginUser)) ;; field user is Object not list need be convert first
  ;(format t "Tokenservice createtoken start2 ~A~%" (datafly:convert-object (slot-value loginUser 'user)))
  ;(jose:encode :hs256 *my$ecret* (datafly:convert-object (slot-value loginUser 'user))) ;;The value "18000000000" is not of type LIST
  (let ((uuid (cl-ppcre:regex-replace-all "-" (write-to-string (uuid:make-v1-uuid)) "")))
    (setf (slot-value loginUser 'token) uuid)
    (setUserAgent tokenService loginUser)
    (refreshToken tokenService loginUser)
    (jose:encode :hs256 *my$ecret* (list (cons 'uuid uuid))))
  )


(defmethod getLoginUser((tokenService TokenService))
  (format t "getloginuser---call~%")
  (let* ((token (getToken))
	 (uuid-token (jose:decode :hs256 *my$ecret* token))
	 (uuid (cdr (car uuid-token)))
	 (userKey (getTokenKey tokenService uuid))
	 (user-json (getCacheObject *RedisCache* userKey))
	 (user (json:with-decoder-simple-clos-semantics
		 (let ((json::*end-of-object-handler* (wrap-for-class 'LoginUser)))
		   (json:decode-json-from-string user-json))))
	 )
    ;(print user)
    ;(format t "token=~A~%" token)
    ;(format t "uuid=~A~%" uuid)
    (format t "user=~A~%" user)
    (break)
    user
    ))


(defun getToken ()
  (format t "getToken---call~%")
  (format t "request-headers---~A~%" (request-headers *request*))
  (maphash #'(lambda (k v) (format t "~A=~A~%" k v)) (request-headers *request*))
  (format t "response-headers---~A~%" (response-headers *response*))
  ;(maphash #'(lambda (k v) (format t "~A=~A~%" k v)) (response-headers *response*)) ;;error response-headers is a list not hashtable
  (let ((authorization (gethash "authorization" (request-headers *request*)))
	(headers (request-headers *request*)))
    (format t "hash-table-p=~A~%" (hash-table-p headers))
    (format t "host=~A~%" (gethash "host" headers))
    (format t "authorization=~A~%" authorization)
    (subseq authorization (length "Bearer ")))) ;;the key is "host" not is 'host or :host


(defmethod setUserAgent ((tokenService TokenService) (loginUser LoginUser))
  ())

(defmethod refreshToken ((tokenService TokenService) (loginUser LoginUser))
  (setf (slot-value loginUser 'loginTime) (get-universal-time))
  (setf (slot-value loginUser 'loginTime) (+ (slot-value loginUser 'loginTime) (* (slot-value tokenService 'expireTime) (slot-value tokenService 'MILLIS_MINUTE))))
  (let ((userKey (getTokenKey tokenService (slot-value loginUser 'token))))
    (format t "xxxxxxxx~A~%" (datafly:object-to-plist loginUser)) ;; can't convert slot user
    (format t "yyyyyyyy~A~%" (datafly:convert-object loginUser)) ;; can't convert slot user
    (format t "zzzzzzzz~A~%" (datafly:encode-json loginUser))
    (format t "wwwwwwww~A~%" (cl-json:encode-json-to-string loginUser))
    (setCacheObject *RedisCache* userKey (cl-json:encode-json-to-string loginUser) (slot-value tokenService 'expireTime) "TimeUnit.MINUTES")
    (if (null (slot-value tokenService 'soloLogin))
	(let ((userIdKey (getUserIdKey tokenService (slot-value (slot-value loginUser 'user) 'user-id))))
	  (setCacheObject *RedisCache* userIdKey userKey (slot-value tokenService 'expireTime) "TimeUnit.MINUTES"))
	()))
  )


(defmethod getTokenKey ((tokenService TokenService) uuid)
  (concatenate 'string (slot-value *Constants* 'LOGIN_TOKEN_KEY) uuid))


(defmethod getUserIdKey ((tokenService TokenService) userId)
  (concatenate 'string (slot-value *Constants* 'PROJECT_NAME) (slot-value *Constants* 'LOGIN_USERID_KEY) (write-to-string userId)))


(defun wrap-for-class (class &optional (fn json::*end-of-object-handler*))
  (let ((prototype (make-instance 'json::prototype :lisp-class class)))
    (lambda ()
      ;; dynamically rebind *prototype* right around calling fn
      (let ((json::*prototype* prototype))
	(format t "*prototype*=~A~%" json::*prototype*)
        (funcall fn)))))

(defun wrap-for-key (&optional (fn json::*object-key-handler*))
  (lambda (key)
    (format t "*object-key-handler*=~A~%" key)
    (format t "*object-key-handler*=~A~%" json::*end-of-object-handler*)
    (format t "*object-key-handler*=~A~%" (equal key "user"))
    ;(if (equal key "user")
    ;	(setf json::*end-of-object-handler* (wrap-for-class 'SysUser)))
    (funcall fn key)
    (if (equal key "user")
	(setf json::*end-of-object-handler* (wrap-for-class 'SysUser)))))
