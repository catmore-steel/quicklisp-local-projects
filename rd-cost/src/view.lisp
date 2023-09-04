(in-package :cl-user)
(defpackage rd-cost.view
  (:use :cl)
  (:import-from :rd-cost.config
                :*template-directory*)
  (:import-from :caveman2
                :*response*
                :response-headers)
  (:import-from :djula
                :add-template-directory
                :compile-template*
                :render-template*
                :*djula-execute-package*)
  (:import-from :mito)
;;  (:import-from :cl-json
;;                :encode-json)
  (:import-from :datafly
                :encode-json)
  (:export :render
           :render-json))
(in-package :rd-cost.view)

(djula:add-template-directory *template-directory*)

(defparameter *template-registry* (make-hash-table :test 'equal))

(defun render (template-path &optional env)
  (let ((template (gethash template-path *template-registry*)))
    (unless template
      (setf template (djula:compile-template* (princ-to-string template-path)))
      (setf (gethash template-path *template-registry*) template))
    (apply #'djula:render-template*
           template nil
           env)))

(defun render-json (object)
  (format t "view:render-json call ~A~%" object)
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (encode-json object))


;;
;; Execute package definition

(defpackage rd-cost.djula
  (:use :cl)
  (:import-from :rd-cost.config
                :config
                :appenv
                :developmentp
                :productionp)
  (:import-from :caveman2
                :url-for))

(setf djula:*djula-execute-package* (find-package :rd-cost.djula))
