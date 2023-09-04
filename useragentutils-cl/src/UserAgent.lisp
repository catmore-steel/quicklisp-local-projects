(in-package :useragentutils-cl)

(defclass UserAgent ()
  ())

(defmethod initialize-instance ((userAgent UserAgent) &key userAgentString)
  (let ((userAgentLowercaseString (if (null userAgentString)
				      nil
				      (string-downcase userAgentString))))
    ()))


(defmethod parseUserAgentString ((userAgent UserAgent) (userAgentString string))
  (make-instance 'UserAgent :userAgentString userAgentString))



