(in-package :rd-cost.web)

(defclass AjaxResult ()
  ((code :initarg :code)
   (msg :initarg :msg)
   (data :initform nil)))

(defmethod success ((ajaxResult AjaxResult))
  (success-arg2 ajaxResult "ok"))

(defmethod success-arg2 ((ajaxResult AjaxResult) msg)
  (success-arg3 ajaxResult msg nil))

(defmethod success-arg3 ((ajaxResult AjaxResult) msg data)
  (let ((ht (make-hash-table)))
    (setf (gethash 'code ht) 200)
    (setf (gethash 'msg ht) msg)
    (setf (gethash 'data ht) data)
    ht))
