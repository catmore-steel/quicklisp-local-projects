(defpackage #:BrowserType
  (:use #:cl)
  (:export #:BrowserType))

(in-package #:BrowserType)


(defenum:defenum (BrowserType (:initargs (namee)))
    ((WEB_BROWSER("Browser"))
     ;(MOBILE_BROWSER("Browser (mobile)"))
     ;(TEXT_BROWSER("Browser (text only)"))
     ;(EMAIL_CLIENT("Email Client"))
     ;(ROBOT("Robot"))
     ;(TOOL("Downloading tool"))
     ;(APP("Application"))
     ;(UNKNOWN("unknown"))

     )
    ((namee))
    (:method m1((browserType BrowserType))
      (format t "m1~A" :namee))
    (:initialize '(:namee "123"))
    (:initialize (format t "456"))
    (:initialize (format t "tag=~A" (BrowserType 'WEB_BROWSER)))
    )


