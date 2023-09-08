(in-package :rd-cost.web)

(defclass RedisCache ()
  ((redisTemplate :initform (lack.session.store.redis:make-redis-store :host "192.168.1.20" :port 6379))))

(defmethod getCacheObject ((redisCache RedisCache) key)
  (lack.session.store.redis:fetch-session *redis* key))

(defmethod deleteObject ((redisCache RedisCache) key)
  (lack.session.store.redis:remove-session *redis* key))

(defmethod setCacheObject ((redisCache RedisCache) key value timeout timeUnit)
  (let ((redisTemplate (slot-value (make-instance 'RedisCache) 'redisTemplate)))
    (setf (slot-value redisTemplate 'expires) timeout)
    (lack.session.store.redis:store-session redisTemplate key value)))
