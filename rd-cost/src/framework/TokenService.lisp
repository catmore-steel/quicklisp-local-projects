(in-package :rd-cost.web)

(defparameter *token* nil)

(defclass TokenService()
  ())

(defmethod createToken ((tokenService TokenService) (loginUser LoginUser))
  (format t "Tokenservice createtoken start ~A~%" (datafly:convert-object loginUser))
  ;(jose:encode :hs256 (ironclad:ascii-string-to-byte-array "my$ecret") (datafly:convert-object loginUser)) ;; field user is Object not list need be convert first
  (let ((uuid (uuid:make-v1-uuid)))
    (setf (slot-value loginUser 'token) uuid)
    (setUserAgent tokenService loginUser)
    ()))


(defmethod getLoginUser((tokenService TokenService))
  (format t "getloginuser---call~%")
  (let ((token (getToken)))
    (format t "token=~A~%" token)))

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
