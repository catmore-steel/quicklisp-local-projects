(in-package :rd-cost.web)

(defclass RedisCache ()
  ())

(defvar *redis* (lack.session.store.redis:make-redis-store :host "192.168.1.20" :port 6379))
(defvar *RedisCache* (make-instance 'RedisCache)) ;; Service or @Component

(defmethod getCacheObject ((redisCache RedisCache) key)
  (lack.session.store.redis:fetch-session *Redis* key))

(defmethod deleteObject ((redisCache RedisCache) key)
  (lack.session.store.redis:remove-session *RedisCache* key))

(defmethod setCacheObject ((redisCache RedisCache) key value timeout timeUnit)
  ;(break)
  (let ((redis (lack.session.store.redis:make-redis-store :host "192.168.1.20" :port 6379 :expires timeout)))
    (lack.session.store.redis:store-session redis key value)))
