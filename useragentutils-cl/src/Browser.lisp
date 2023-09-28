(defpackage #:Browser
  (:use #:cl #:Manufacturer #:BrowserType #:RenderingEngine)
  (:export #:Browser))

(in-package #:Browser)


(defenum:defenum (BrowserBase (:initargs (manufacturer parent versionId namee aliases exclude browserType renderingEngine versionRegexString)))
    ((OUTLOOK((Manufacturer Manufacturer::MICROSOFT) nil 100 "Outlook" (make-array 1 :initial-element "MSOffice") nil
	      (BrowserType BrowserType::EMAIL_CLIENT) (RenderingEngine RenderingEngine::WORD) "MSOffice (([0-9]+))"))
     (IE((Manufacturer Manufacturer::MICROSOFT) nil 1 "Internet Explorer" (make-array 3 :initial-contents '("MSIE" "Trident" "IE ")) (make-array 3 :initial-contents '("BingPreview" "Xbox" "Xbox One"))
	 (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::TRIDENT) "MSIE (([\\d]+)\\.([\\w]+))" ))
     (EDGE((Manufacturer Manufacturer::MICROSOFT) nil 300 "Microsoft Edge" (make-array 1 :initial-element "Edge") nil
	   (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::TRIDENT) "(?:Edge\\/(([0-9]+)\\.([0-9]*)))"))
     (CHROME((Manufacturer Manufacturer::GOOGLE) nil 1 "Chrome" (make-array 3 :initial-contents '("Chrome" "CrMo" "CriOS")) (make-array 3 :initial-contents '("OPR/" "Web Preview" "Vivaldi"))
	     (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "Chrome\\/(([0-9]+)\\.?([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)" ))
     (FIREFOX((Manufacturer Manufacturer::MOZILLA) nil 10 "Firefox" (make-array 2 :initial-contents '("Firefox" "FxiOS"))
	     (make-array 4 :initial-contents '("camino" "flock" "ggpht.com" "WordPress.com mShots"))
	      (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) "Firefox\\/(([0-9]+)\\.?([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     (SAFARI((Manufacturer Manufacturer::APPLE) nil 1 "Safari" (make-array 1 :initial-element "Safari")
	     (make-array 7 :initial-contents '("bot" "preview" "OPR/" "Coast/" "Vivaldi" "CFNetwork" "Phantom"))
	     (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "Version\\/(([0-9]+)\\.?([\\w]+)?(\\.[\\w]+)?)" ))
     (COAST((Manufacturer Manufacturer::OPERA) nil 500 "Opera" (make-array 1 :initial-element " Coast/") nil
	    (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "Coast\\/(([\\d]+)\\.([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     (OPERA((Manufacturer Manufacturer::OPERA) nil 1 "Opera" (make-array 2 :initial-contents '(" OPR/" "Opera")) nil
	    (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::PRESTO) "[o][p][e]?[r][a]?\\/(([\\d]+)\\.([\\w]+)(\\.([\\w]+))?(\\.([\\w]+))?)"))
     (APPLE_WEB_KIT((Manufacturer Manufacturer::APPLE) nil 50 "Apple WebKit" (make-array 1 :initial-element "AppleWebKit")
		    (make-array 6 :initial-contents '("bot" "preview" "OPR/" "Coast/" "Vivaldi" "Phantom"))
		    (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CAMINO((Manufacturer Manufacturer::OTHER) nil 5 "Camino" (make-array 1 :initial-element "Camino") nil
	     (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) "Camino\\/(([0-9]+)\\.?([\\w]+)?(\\.[\\w]+)?)" ))
     (THUNDERBIRD((Manufacturer Manufacturer::MOZILLA) nil 110 "Thunderbird" (make-array 1 :initial-element "Thunderbird") nil
		  (BrowserType BrowserType::EMAIL_CLIENT) (RenderingEngine RenderingEngine::GECKO) "Thunderbird\\/(([0-9]+)\\.?([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)" ))
     (BOT((Manufacturer Manufacturer::OTHER) nil 12 "Robot/Spider"
	  (make-array 22 :initial-contents '("Googlebot" "Mediapartners-Google" "Web Preview" "bot" "Applebot" "spider" "crawler" "Feedfetcher" "Slurp" "Twiceler" "Nutch" "BecomeBot" "bingbot"
					     "BingPreview" "Google Web Preview" "WordPress.com mShots" "Seznam" "facebookexternalhit" "YandexMarket" "Teoma" "ThumbSniper" "Phantom")) nil
	  (BrowserType BrowserType::ROBOT) (RenderingEngine RenderingEngine::OTHER) nil))
     
     )

    ((manufacturer)
     (parent)
     (versionId)
     (namee)
     (aliases)
     (exclude)
     (browserType)
     (renderingEngine)
     (versionRegexString)

     (id)
     (topLevelBrowsers)
     )
    (:method init (instant)
      (format t "init---~A=~%" instant)
      (let* ((a (defenum:tag-of instant 'OUTLOOK))
	     (a2 (BrowserBase 'OUTLOOK))
	     (a3 (BrowserBase-manufacturer a))
	     (a4 (Manufacturer-id a3))
	    )
	(format t "get outlook ~A~%" a)
	(format t "get outlook ~A~%" a2)
	(format t "get outlook manufacturer ~A~%" a3)
	(format t "get outlook manufacturer id ~A~%" a4)
	;(setf (BrowserBase-id (BrowserBase 'OUTLOOK)) 2)
	;(format t "~A~%" (BrowserBase-id ))
	 )
      )
    
    (:method m1 ((browserBase BrowserBase) arg1)
       (format t "m1~%"))
    (:method m2 ((browserBase BrowserBase) arg2)
       (format t "m2~%"))
    (:initialize (format t "123~%") (format t "789~%"))
    (:initialize (format t "456~%"))
    (:initialize (format t "enum=~A~%" (defenum:find-enum 'BrowserBase)))
    (:initialize (format t "tag=~A~%" (BrowserBase 'OUTLOOK)))
    ;(:initialize (format t "xxx=~A~%" BROWSERBASE/INSTANCE-manufacturer))









    (:initialize (init (defenum:find-enum 'BrowserBase)))
    (:initialize :id 1)
    (:initialize (list :id 2))
    (:initialize (format t "id=~A~%" (getf (list :id 1) :id)))
    ;(:initialize (format t "xxx=~A~%" (getf (defenum:find-enum 'BrowserBase) :id)))
    )

(defenum:defenum (Browser-FIREFOX_MOBILE (:initargs (manufacturer parent versionId namee aliases exclude browserType renderingEngine versionRegexString)))
    (
   
     (FIREFOX_MOBILE((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 200 "Firefox Mobile" (make-array 1 :initial-element "Mobile") nil
		     (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     
     )

    ((manufacturer)
     (parent)
     (versionId)
     (namee)
     (aliases)
     (exclude)
     (browserType)
     (renderingEngine)
     (versionRegexString)

   ;;(topLevelBrowsers)
     )
    
    )

(defenum:defenum (Browser-EDGE_MOBILE (:initargs (manufacturer parent versionId namee aliases exclude browserType renderingEngine versionRegexString)))
    ((EDGE_MOBILE((Manufacturer Manufacturer::MICROSOFT) (BrowserBase 'EDGE) 304 "Microsoft Edge Mobile" (make-array 1 :initial-element "Mobile Safari") nil
		  (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::EDGE_HTML) nil ))
     
     )

    ((manufacturer)
     (parent)
     (versionId)
     (namee)
     (aliases)
     (exclude)
     (browserType)
     (renderingEngine)
     (versionRegexString)

   ;;(topLevelBrowsers)
     )
    
    )


(defenum:defenum (Browser (:initargs (manufacturer parent versionId namee aliases exclude browserType renderingEngine versionRegexString))
			  
			  )
    ((OUTLOOK((Manufacturer Manufacturer::MICROSOFT) nil 100 "Outlook" (make-array 1 :initial-element "MSOffice") nil
	      (BrowserType BrowserType::EMAIL_CLIENT) (RenderingEngine RenderingEngine::WORD) "MSOffice (([0-9]+))"))
     (OUTLOOK2007((Manufacturer Manufacturer::MICROSOFT) (BrowserBase 'OUTLOOK) 107 "Outlook 2007" (make-array 1 :initial-element "MSOffice 12") nil
		  (BrowserType BrowserType::EMAIL_CLIENT) (RenderingEngine RenderingEngine::WORD) nil))
     (OUTLOOK2013((Manufacturer Manufacturer::MICROSOFT) (BrowserBase 'OUTLOOK) 109 "Outlook 2013" (make-array 1 :initial-element "Microsoft Outlook 15") nil
		  (BrowserType BrowserType::EMAIL_CLIENT) (RenderingEngine RenderingEngine::WORD) nil))
     (OUTLOOK2010((Manufacturer Manufacturer::MICROSOFT) (BrowserBase 'OUTLOOK) 108 "Outlook 2010" (make-array 2 :initial-contents '("MSOffice 14" "Microsoft Outlook 14")) nil
		  (BrowserType BrowserType::EMAIL_CLIENT) (RenderingEngine RenderingEngine::WORD) nil))
     
     ;(IE((Manufacturer Manufacturer::MICROSOFT) nil 1 "Internet Explorer" (make-array 3 :initial-contents '("MSIE" "Trident" "IE ")) (make-array 3 :initial-contents '("BingPreview" "Xbox" "Xbox One"))
     ;	 (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::TRIDENT) "MSIE (([\\d]+)\\.([\\w]+))" ))
     (OUTLOOK_EXPRESS7((Manufacturer Manufacturer::MICROSOFT) (BrowserBase 'IE) 110 "Windows Live Mail" (make-array 1 :initial-element "Outlook-Express/7.0") nil
		       (BrowserType BrowserType::EMAIL_CLIENT) (RenderingEngine RenderingEngine::TRIDENT) nil))
     (IEMOBILE11((Manufacturer Manufacturer::MICROSOFT) (BrowserBase 'IE) 125 "IE Mobile 11" (make-array 1 :initial-element "IEMobile/11") nil
		 (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::TRIDENT) nil))
     (IEMOBILE10((Manufacturer Manufacturer::MICROSOFT) (BrowserBase 'IE) 124 "IE Mobile 10" (make-array 1 :initial-element "IEMobile/10") nil
		 (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::TRIDENT) nil))
     (IEMOBILE9((Manufacturer Manufacturer::MICROSOFT) (BrowserBase 'IE) 123 "IE Mobile 9" (make-array 1 :initial-element "IEMobile/9") nil
		(BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::TRIDENT) nil))
     (IEMOBILE7((Manufacturer Manufacturer::MICROSOFT) (BrowserBase 'IE) 121 "IE Mobile 7" (make-array 1 :initial-element "IEMobile 7") nil
		(BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::TRIDENT) nil))
     (IEMOBILE6((Manufacturer Manufacturer::MICROSOFT) (BrowserBase 'IE) 120 "IE Mobile 6" (make-array 1 :initial-element "IEMobile 6") nil
		(BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::TRIDENT) nil))
     (IE_XBOX((Manufacturer Manufacturer::MICROSOFT) (BrowserBase 'IE) 360 "Xbox" (make-array 1 :initial-element "xbox") (make-array 1 :initial-element nil)
	      (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::TRIDENT) nil))
     (IE11((Manufacturer Manufacturer::MICROSOFT) (BrowserBase 'IE) 95 "Internet Explorer 11" (make-array 2 :initial-contents '("Trident/7" "IE 11."))
	   (make-array 2 :initial-contents '("MSIE 7" "BingPreview"))
	   (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::TRIDENT) "(?:Trident\\/7|IE)(?:\\.[0-9]*;)?(?:.*rv:| )(([0-9]+)\\.?([0-9]+))" ))
     (IE10((Manufacturer Manufacturer::MICROSOFT) (BrowserBase 'IE) 92 "Internet Explorer 10" (make-array 1 :initial-element "MSIE 10") nil
	   (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::TRIDENT) nil ))
     (IE9((Manufacturer Manufacturer::MICROSOFT) (BrowserBase 'IE) 90 "Internet Explorer 9" (make-array 1 :initial-element "MSIE 9") nil
	  (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::TRIDENT) nil ))
     (IE8((Manufacturer Manufacturer::MICROSOFT) (BrowserBase 'IE) 80 "Internet Explorer 8" (make-array 1 :initial-element "MSIE 8") nil
	  (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::TRIDENT) nil ))
     (IE7((Manufacturer Manufacturer::MICROSOFT) (BrowserBase 'IE) 70 "Internet Explorer 7" (make-array 1 :initial-element "MSIE 7") nil
	  (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::TRIDENT) nil))
     (IE6((Manufacturer Manufacturer::MICROSOFT) (BrowserBase 'IE) 60 "Internet Explorer 6" (make-array 1 :initial-element "MSIE 6") nil
	  (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::TRIDENT) nil ))
     (IE5_5((Manufacturer Manufacturer::MICROSOFT) (BrowserBase 'IE) 55 "Internet Explorer 5.5" (make-array 1 :initial-element "MSIE 5.5") nil
	    (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::TRIDENT) nil))
     (IE5((Manufacturer Manufacturer::MICROSOFT) (BrowserBase 'IE) 50 "Internet Explorer 5" (make-array 1 :initial-element "MSIE 5") nil
	  (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::TRIDENT) nil ))
     
     ;(EDGE((Manufacturer Manufacturer::MICROSOFT) nil 300 "Microsoft Edge" (make-array 1 :initial-element "Edge") nil
     ;	   (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::TRIDENT) "(?:Edge\\/(([0-9]+)\\.([0-9]*)))"))
     ;(EDGE_MOBILE((Manufacturer Manufacturer::MICROSOFT) (BrowserBase 'EDGE) 304 "Microsoft Edge Mobile" (make-array 1 :initial-element "Mobile Safari") nil
     ;		  (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::EDGE_HTML) nil ))
     (EDGE_MOBILE12((Manufacturer Manufacturer::MICROSOFT) (Browser-EDGE_MOBILE 'EDGE_MOBILE) 302 "Microsoft Edge Mobile 12" (make-array 2 :initial-contents '("Mobile Safari" "Edge/12")) nil
		    (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::EDGE_HTML) nil ))
     (EDGE13((Manufacturer Manufacturer::MICROSOFT) (BrowserBase 'EDGE) 303 "Microsoft Edge 13" (make-array 1 :initial-element "Edge/13") (make-array 1 :initial-element "Mobile")
	     (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::EDGE_HTML) nil))
     (EDGE12((Manufacturer Manufacturer::MICROSOFT) (BrowserBase 'EDGE) 301 "Microsoft Edge 12" (make-array 1 :initial-element "Edge/12") (make-array 1 :initial-element "Mobile")
	     (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::EDGE_HTML) nil ))
     
     ;; Google Chrome browser
     ;(CHROME((Manufacturer Manufacturer::GOOGLE) nil 1 "Chrome" (make-array 3 :initial-contents '("Chrome" "CrMo" "CriOS")) (make-array 3 :initial-contents '("OPR/" "Web Preview" "Vivaldi"))
     ;	     (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "Chrome\\/(([0-9]+)\\.?([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)" ))
     (CHROME_MOBILE((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 100 "Chrome Mobile" (make-array 3 :initial-contents '("CrMo" "CriOS" "Mobile Safari"))
		    (make-array 1 :initial-element "OPR/")
		    (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "(?:CriOS|CrMo|Chrome)\\/(([0-9]+)\\.?([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)" ))
     (CHROME49((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 55 "Chrome 49" (make-array 1 :initial-element "Chrome/49") (make-array 3 :initial-contents '("OPR/" "Web Preview" "Vivaldi" ))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil ))
     (CHROME48((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 53 "Chrome 48" (make-array 1 :initial-element "Chrome/48") (make-array 3 :initial-contents '("OPR/" "Web Preview" "Vivaldi"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil ))
     (CHROME47((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 52 "Chrome 47" (make-array 1 :initial-element "Chrome/47") (make-array 3 :initial-contents '("OPR/" "Web Preview" "Vivaldi"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil ))
     (CHROME46((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 51 "Chrome 46" (make-array 1 :initial-element "Chrome/46") (make-array 3 :initial-contents '("OPR/" "Web Preview" "Vivaldi"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil ))
     (CHROME45((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 50 "Chrome 45" (make-array 1 :initial-element "Chrome/45") (make-array 3 :initial-contents '("OPR/" "Web Preview" "Vivaldi"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil ))
     (CHROME44((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 49 "Chrome 44" (make-array 1 :initial-element "Chrome/44") (make-array 3 :initial-contents '("OPR/" "Web Preview" "Vivaldi"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil ))
     (CHROME43((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 48 "Chrome 43" (make-array 1 :initial-element "Chrome/43") (make-array 3 :initial-contents '("OPR/" "Web Preview" "Vivaldi"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil ))
     (CHROME42((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 47 "Chrome 42" (make-array 1 :initial-element "Chrome/42") (make-array 3 :initial-contents '("OPR/" "Web Preview" "Vivaldi"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil ))
     (CHROME41((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 46 "Chrome 41" (make-array 1 :initial-element "Chrome/41") (make-array 3 :initial-contents '("OPR/" "Web Preview" "Vivaldi"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil ))
     (CHROME40((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 45 "Chrome 40" (make-array 1 :initial-element "Chrome/40") (make-array 3 :initial-contents '("OPR/" "Web Preview" "Vivaldi"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil ))
     (CHROME39((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 44 "Chrome 39" (make-array 1 :initial-element "Chrome/39") (make-array 2 :initial-contents '("OPR/" "Web Preview"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME38((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 43 "Chrome 38" (make-array 1 :initial-element "Chrome/38") (make-array 2 :initial-contents '("OPR/" "Web Preview"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME37((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 42 "Chrome 37" (make-array 1 :initial-element "Chrome/37") (make-array 2 :initial-contents '("OPR/" "Web Preview"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME36((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 41 "Chrome 36" (make-array 1 :initial-element "Chrome/36") (make-array 2 :initial-contents '("OPR/" "Web Preview"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME35((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 40 "Chrome 35" (make-array 1 :initial-element "Chrome/35") (make-array 2 :initial-contents '("OPR/" "Web Preview"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME34((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 39 "Chrome 34" (make-array 1 :initial-element "Chrome/34") (make-array 2 :initial-contents '("OPR/" "Web Preview"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME33((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 38 "Chrome 33" (make-array 1 :initial-element "Chrome/33") (make-array 2 :initial-contents '("OPR/" "Web Preview"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME32((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 37 "Chrome 32" (make-array 1 :initial-element "Chrome/32") (make-array 2 :initial-contents '("OPR/" "Web Preview"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME31((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 36 "Chrome 31" (make-array 1 :initial-element "Chrome/31") (make-array 2 :initial-contents '("OPR/" "Web Preview"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME30((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 35 "Chrome 30" (make-array 1 :initial-element "Chrome/30") (make-array 2 :initial-contents '("OPR/" "Web Preview"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME29((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 34 "Chrome 29" (make-array 1 :initial-element "Chrome/29") (make-array 2 :initial-contents '("OPR/" "Web Preview"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME28((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 33 "Chrome 28" (make-array 1 :initial-element "Chrome/28") (make-array 2 :initial-contents '("OPR/" "Web Preview"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME27((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 32 "Chrome 27" (make-array 1 :initial-element "Chrome/27") (make-array 2 :initial-contents '("OPR/" "Web Preview"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME26((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 31 "Chrome 26" (make-array 1 :initial-element "Chrome/26") (make-array 2 :initial-contents '("OPR/" "Web Preview"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME25((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 30 "Chrome 25" (make-array 1 :initial-element "Chrome/25") (make-array 2 :initial-contents '("OPR/" "Web Preview"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME24((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 29 "Chrome 24" (make-array 1 :initial-element "Chrome/24") (make-array 2 :initial-contents '("OPR/" "Web Preview"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME23((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 28 "Chrome 23" (make-array 1 :initial-element "Chrome/23") (make-array 2 :initial-contents '("OPR/" "Web Preview"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME22((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 27 "Chrome 22" (make-array 1 :initial-element "Chrome/22") (make-array 2 :initial-contents '("OPR/" "Web Preview"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME21((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 26 "Chrome 21" (make-array 1 :initial-element "Chrome/21") (make-array 2 :initial-contents '("OPR/" "Web Preview"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME20((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 25 "Chrome 20" (make-array 1 :initial-element "Chrome/20") (make-array 2 :initial-contents '("OPR/" "Web Preview"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME19((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 24 "Chrome 19" (make-array 1 :initial-element "Chrome/19") (make-array 2 :initial-contents '("OPR/" "Web Preview"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME18((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 23 "Chrome 18" (make-array 1 :initial-element "Chrome/18") nil
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME17((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 22 "Chrome 17" (make-array 1 :initial-element "Chrome/17") nil
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME16((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 21 "Chrome 16" (make-array 1 :initial-element "Chrome/16") nil
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME15((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 20 "Chrome 15" (make-array 1 :initial-element "Chrome/15") nil
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME14((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 19 "Chrome 14" (make-array 1 :initial-element "Chrome/14") nil
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME13((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 18 "Chrome 13" (make-array 1 :initial-element "Chrome/13") nil
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME12((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 17 "Chrome 12" (make-array 1 :initial-element "Chrome/12") nil
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME11((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 16 "Chrome 11" (make-array 1 :initial-element "Chrome/11") nil
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME10((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 15 "Chrome 10" (make-array 1 :initial-element "Chrome/10") nil
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME9((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 10 "Chrome 9" (make-array 1 :initial-element "Chrome/9") nil
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (CHROME8((Manufacturer Manufacturer::GOOGLE) (BrowserBase 'CHROME) 5 "Chrome 8" (make-array 1 :initial-element "Chrome/8") nil
	      (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))

     (OMNIWEB((Manufacturer Manufacturer::OTHER) nil 2 "Omniweb" (make-array 1 :initial-element "OmniWeb") nil
	      (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))

     ;(FIREFOX((Manufacturer Manufacturer::MOZILLA) nil 10 "Firefox" (make-array 2 :initial-contents '("Firefox" "FxiOS"))
     ;	      (make-array 4 :initial-contents '("camino" "flock" "ggpht.com" "WordPress.com mShots"))
     ;	      (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) "Firefox\\/(([0-9]+)\\.?([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     (FIREFOX3MOBILE((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 31 "Firefox 3 Mobile" (make-array 1 :initial-element "Firefox/3.5 Maemo") nil
		     (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     ;(FIREFOX_MOBILE((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 200 "Firefox Mobile" (make-array 1 :initial-element "Mobile") nil
     ;		     (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX_MOBILE23((Manufacturer Manufacturer::MOZILLA) (Browser-FIREFOX_MOBILE 'FIREFOX_MOBILE) 223 "Firefox Mobile 23" (make-array 1 :initial-element "Firefox/23") nil
		       (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     ;;Firefox for iOS devices. This Firefox version is using webkit instead of gecko rendering engine.
     (FIREFOX_MOBILE_IOS((Manufacturer Manufacturer::MOZILLA) (Browser-FIREFOX_MOBILE 'FIREFOX_MOBILE) 224 "Firefox Mobile (iOS)" (make-array 1 :initial-element "FxiOS") nil
			 (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil ))
     (FIREFOX45((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 222 "Firefox 45" (make-array 1 :initial-element "Firefox/45") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX44((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 221 "Firefox 44" (make-array 1 :initial-element "Firefox/44") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX43((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 220 "Firefox 43" (make-array 1 :initial-element "Firefox/43") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX42((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 219 "Firefox 42" (make-array 1 :initial-element "Firefox/42") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX41((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 218 "Firefox 41" (make-array 1 :initial-element "Firefox/41") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX40((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 217 "Firefox 40" (make-array 1 :initial-element "Firefox/40") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX39((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 216 "Firefox 39" (make-array 1 :initial-element "Firefox/39") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX38((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 215 "Firefox 38" (make-array 1 :initial-element "Firefox/38") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX37((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 214 "Firefox 37" (make-array 1 :initial-element "Firefox/37") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX36((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 213 "Firefox 36" (make-array 1 :initial-element "Firefox/36") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX35((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 212 "Firefox 35" (make-array 1 :initial-element "Firefox/35") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX34((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 211 "Firefox 34" (make-array 1 :initial-element "Firefox/34") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX33((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 210 "Firefox 33" (make-array 1 :initial-element "Firefox/33") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX32((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 109 "Firefox 32" (make-array 1 :initial-element "Firefox/32") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX31((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 310 "Firefox 31" (make-array 1 :initial-element "Firefox/31") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX30((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 300 "Firefox 30" (make-array 1 :initial-element "Firefox/30") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX29((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 290 "Firefox 29" (make-array 1 :initial-element "Firefox/29") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX28((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 280 "Firefox 28" (make-array 1 :initial-element "Firefox/28") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX27((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 108 "Firefox 27" (make-array 1 :initial-element "Firefox/27") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX26((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 107 "Firefox 26" (make-array 1 :initial-element "Firefox/26") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX25((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 106 "Firefox 25" (make-array 1 :initial-element "Firefox/25") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX24((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 105 "Firefox 24" (make-array 1 :initial-element "Firefox/24") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX23((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 104 "Firefox 23" (make-array 1 :initial-element "Firefox/23") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX22((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 103 "Firefox 22" (make-array 1 :initial-element "Firefox/22") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX21((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 102 "Firefox 21" (make-array 1 :initial-element "Firefox/21") (make-array 1 :initial-element "WordPress.com mShots")
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX20((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 101 "Firefox 20" (make-array 1 :initial-element "Firefox/20") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX19((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 100 "Firefox 19" (make-array 1 :initial-element "Firefox/19") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX18((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 99 "Firefox 18" (make-array 1 :initial-element "Firefox/18") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX17((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 98 "Firefox 17" (make-array 1 :initial-element "Firefox/17") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX16((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 97 "Firefox 16" (make-array 1 :initial-element "Firefox/16") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX15((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 96 "Firefox 15" (make-array 1 :initial-element "Firefox/15") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX14((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 95 "Firefox 14" (make-array 1 :initial-element "Firefox/14") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX13((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 94 "Firefox 13" (make-array 1 :initial-element "Firefox/13") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX12((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 93 "Firefox 12" (make-array 1 :initial-element "Firefox/12") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX11((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 92 "Firefox 11" (make-array 1 :initial-element "Firefox/11") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX10((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 91 "Firefox 10" (make-array 1 :initial-element "Firefox/10") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX9((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 90 "Firefox 9" (make-array 1 :initial-element "Firefox/9") nil
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX8((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 80 "Firefox 8" (make-array 1 :initial-element "Firefox/8") nil
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX7((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 70 "Firefox 7" (make-array 1 :initial-element "Firefox/7") nil
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX6((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 60 "Firefox 6." (make-array 1 :initial-element "Firefox/6.") nil
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX5((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 50 "Firefox 5." (make-array 1 :initial-element "Firefox/5.") nil
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX4((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 40 "Firefox 4." (make-array 1 :initial-element "Firefox/4.") nil
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX3((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 30 "Firefox 3." (make-array 1 :initial-element "Firefox/3.") (make-array 3 :initial-contents '("Camino" "Flock" "ggpht.com"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX2((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 20 "Firefox 2."
	       (make-array 1 :initial-element "Firefox/2.") (make-array 2 :initial-contents '("Camino" "WordPress.com mShots"))
	       (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     (FIREFOX1_5((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'FIREFOX) 15 "Firefox 1.5" (make-array 1 :initial-element "Firefox/1.5") nil
		 (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil ))
     
     ;(SAFARI((Manufacturer Manufacturer::APPLE) nil 1 "Safari" (make-array 1 :initial-element "Safari")
     ;	     (make-array 7 :initial-contents '("bot" "preview" "OPR/" "Coast/" "Vivaldi" "CFNetwork" "Phantom"))
     ;	     (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "Version\\/(([0-9]+)\\.?([\\w]+)?(\\.[\\w]+)?)" ))
     (BLACKBERRY10((Manufacturer Manufacturer::BLACKBERRY) (BrowserBase 'SAFARI) 10 "BlackBerry" (make-array 1 :initial-element "BB10") nil
		   (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (MOBILE_SAFARI((Manufacturer Manufacturer::APPLE) (BrowserBase 'SAFARI) 2 "Mobile Safari" (make-array 2 :initial-contents '("Mobile Safari" "Mobile/"))
		    (make-array 7 :initial-contents '("bot" "preview" "OPR/" "Coast/" "Vivaldi" "CFNetwork" "FxiOS"))
		    (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil ))
     (SILK((Manufacturer Manufacturer::AMAZON) (BrowserBase 'SAFARI) 15 "Silk" (make-array 1 :initial-element "Silk/") nil
	   (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "Silk\\/(([0-9]+)\\.?([\\w]+)?(\\.[\\w]+)?(\\-[\\w]+)?)" ))
     (SAFARI9((Manufacturer Manufacturer::APPLE) (BrowserBase 'SAFARI) 9 "Safari 9" (make-array 1 :initial-element  "Version/9") (make-array 1 :initial-element "Applebot")
	      (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil ))
     (SAFARI8((Manufacturer Manufacturer::APPLE) (BrowserBase 'SAFARI) 8 "Safari 8" (make-array 1 :initial-element  "Version/8") (make-array 1 :initial-element "Applebot")
	      (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil ))
     (SAFARI7((Manufacturer Manufacturer::APPLE) (BrowserBase 'SAFARI) 7 "Safari 7" (make-array 1 :initial-element  "Version/7") (make-array 1 :initial-element "bing")
	      (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil ))
     (SAFARI6((Manufacturer Manufacturer::APPLE) (BrowserBase 'SAFARI) 6 "Safari 6" (make-array 1 :initial-element  "Version/6") nil
	      (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil ))
     (SAFARI5((Manufacturer Manufacturer::APPLE) (BrowserBase 'SAFARI) 3 "Safari 5" (make-array 1 :initial-element  "Version/5") (make-array 1 :initial-element "Google Web Preview")
	      (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil ))
     (SAFARI4((Manufacturer Manufacturer::APPLE) (BrowserBase 'SAFARI) 4 "Safari 4" (make-array 1 :initial-element  "Version/4") (make-array 1 :initial-element "Googlebot-Mobile")
	      (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil ))

     ;;Opera Coast mobile browser, http://en.wikipedia.org/wiki/Opera_Coast
     ;(COAST((Manufacturer Manufacturer::OPERA) nil 500 "Opera" (make-array 1 :initial-element " Coast/") nil
     ;	    (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "Coast\\/(([\\d]+)\\.([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     (COAST1((Manufacturer Manufacturer::OPERA) (BrowserBase 'COAST) 501 "Opera" (make-array 1 :initial-element " Coast/1.") nil
	     (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "Coast\\/(([\\d]+)\\.([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     ;(OPERA((Manufacturer Manufacturer::OPERA) nil 1 "Opera" (make-array 2 :initial-contents '(" OPR/" "Opera")) nil
     ;	    (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::PRESTO) "[o][p][e]?[r][a]?\\/(([\\d]+)\\.([\\w]+)(\\.([\\w]+))?(\\.([\\w]+))?)"))
     (OPERA_MOBILE((Manufacturer Manufacturer::OPERA) (BrowserBase 'OPERA) 100 "Opera Mobile" (make-array 1 :initial-element "Mobile Safari") nil
		   (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::BLINK) "OPR\\/(([\\d]+)\\.([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     (OPERA_MINI((Manufacturer Manufacturer::OPERA) (BrowserBase 'OPERA) 20 "Opera Mini" (make-array 1 :initial-element "Opera Mini") nil
		 (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::PRESTO) nil))
     (OPERA34((Manufacturer Manufacturer::OPERA) (BrowserBase 'OPERA) 34 "Opera 34" (make-array 1 :initial-element "OPR/34.") nil
	      (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "OPR\\/(([\\d]+)\\.([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     (OPERA33((Manufacturer Manufacturer::OPERA) (BrowserBase 'OPERA) 33 "Opera 33" (make-array 1 :initial-element "OPR/33.") nil
	      (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "OPR\\/(([\\d]+)\\.([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     (OPERA32((Manufacturer Manufacturer::OPERA) (BrowserBase 'OPERA) 32 "Opera 32" (make-array 1 :initial-element "OPR/32.") nil
	      (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "OPR\\/(([\\d]+)\\.([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     (OPERA31((Manufacturer Manufacturer::OPERA) (BrowserBase 'OPERA) 31 "Opera 31" (make-array 1 :initial-element "OPR/31.") nil
	      (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "OPR\\/(([\\d]+)\\.([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     (OPERA30((Manufacturer Manufacturer::OPERA) (BrowserBase 'OPERA) 30 "Opera 30" (make-array 1 :initial-element "OPR/30.") nil
	      (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "OPR\\/(([\\d]+)\\.([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     (OPERA29((Manufacturer Manufacturer::OPERA) (BrowserBase 'OPERA) 29 "Opera 29" (make-array 1 :initial-element "OPR/29.") nil
	      (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "OPR\\/(([\\d]+)\\.([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     (OPERA28((Manufacturer Manufacturer::OPERA) (BrowserBase 'OPERA) 28 "Opera 28" (make-array 1 :initial-element "OPR/28.") nil
	      (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "OPR\\/(([\\d]+)\\.([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     (OPERA27((Manufacturer Manufacturer::OPERA) (BrowserBase 'OPERA) 27 "Opera 27" (make-array 1 :initial-element "OPR/27.") nil
	      (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "OPR\\/(([\\d]+)\\.([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     (OPERA26((Manufacturer Manufacturer::OPERA) (BrowserBase 'OPERA) 26 "Opera 26" (make-array 1 :initial-element "OPR/26.") nil
	      (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "OPR\\/(([\\d]+)\\.([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     (OPERA25((Manufacturer Manufacturer::OPERA) (BrowserBase 'OPERA) 25 "Opera 25" (make-array 1 :initial-element "OPR/25.") nil
	      (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "OPR\\/(([\\d]+)\\.([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     (OPERA24((Manufacturer Manufacturer::OPERA) (BrowserBase 'OPERA) 24 "Opera 24" (make-array 1 :initial-element "OPR/24.") nil
	      (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "OPR\\/(([\\d]+)\\.([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     (OPERA23((Manufacturer Manufacturer::OPERA) (BrowserBase 'OPERA) 23 "Opera 23" (make-array 1 :initial-element "OPR/23.") nil
	      (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "OPR\\/(([\\d]+)\\.([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     (OPERA22((Manufacturer Manufacturer::OPERA) (BrowserBase 'OPERA) 22 "Opera 22" (make-array 1 :initial-element "OPR/22.") nil
	      (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "OPR\\/(([\\d]+)\\.([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     (OPERA21((Manufacturer Manufacturer::OPERA) (BrowserBase 'OPERA) 21 "Opera 21" (make-array 1 :initial-element "OPR/21.") nil
	      (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "OPR\\/(([\\d]+)\\.([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     (OPERA20((Manufacturer Manufacturer::OPERA) (BrowserBase 'OPERA) 20 "Opera 20" (make-array 1 :initial-element "OPR/20.") nil
	      (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "OPR\\/(([\\d]+)\\.([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     (OPERA19((Manufacturer Manufacturer::OPERA) (BrowserBase 'OPERA) 19 "Opera 19" (make-array 1 :initial-element "OPR/19.") nil
	      (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "OPR\\/(([\\d]+)\\.([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     (OPERA18((Manufacturer Manufacturer::OPERA) (BrowserBase 'OPERA) 18 "Opera 18" (make-array 1 :initial-element "OPR/18.") nil
	      (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "OPR\\/(([\\d]+)\\.([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     (OPERA17((Manufacturer Manufacturer::OPERA) (BrowserBase 'OPERA) 17 "Opera 17" (make-array 1 :initial-element "OPR/17.") nil
	      (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "OPR\\/(([\\d]+)\\.([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     (OPERA16((Manufacturer Manufacturer::OPERA) (BrowserBase 'OPERA) 16 "Opera 16" (make-array 1 :initial-element "OPR/16.") nil
	      (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "OPR\\/(([\\d]+)\\.([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     (OPERA15((Manufacturer Manufacturer::OPERA) (BrowserBase 'OPERA) 15 "Opera 15" (make-array 1 :initial-element "OPR/15.") nil
	      (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "OPR\\/(([\\d]+)\\.([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     (OPERA12((Manufacturer Manufacturer::OPERA) (BrowserBase 'OPERA) 12 "Opera 12" (make-array 2 :initial-contents '("Opera/12" "Version/12.")) nil
	      (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "OPR\\/(([\\d]+)\\.([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     (OPERA11((Manufacturer Manufacturer::OPERA) (BrowserBase 'OPERA) 11 "Opera 11" (make-array 1 :initial-element "Version/11.") nil
	      (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "OPR\\/(([\\d]+)\\.([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     (OPERA10((Manufacturer Manufacturer::OPERA) (BrowserBase 'OPERA) 10 "Opera 10" (make-array 1 :initial-element "Opera/9.8") nil
	      (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "OPR\\/(([\\d]+)\\.([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))
     (OPERA9((Manufacturer Manufacturer::OPERA) (BrowserBase 'OPERA) 9 "Opera 9" (make-array 1 :initial-element "Opera/9") nil
	      (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) "OPR\\/(([\\d]+)\\.([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"))

     (KONQUEROR((Manufacturer Manufacturer::OTHER) nil 1 "Konqueror" (make-array 1 :initial-element "Konqueror") (make-array 1 :initial-element "Exabot")
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::KHTML) "Konqueror\\/(([0-9]+)\\.?([\\w]+)?(-[\\w]+)?)" ))
     (DOLFIN2((Manufacturer Manufacturer::SAMSUNG) nil 1 "Samsung Dolphin 2" (make-array 1 :initial-element "Dolfin/2") nil
	      (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))

     ;;Apple WebKit compatible client. Can be a browser or an application with embedded browser using UIWebView.
     ;(APPLE_WEB_KIT((Manufacturer Manufacturer::APPLE) nil 50 "Apple WebKit" (make-array 1 :initial-element "AppleWebKit")
     ;		    (make-array 6 :initial-contents '("bot" "preview" "OPR/" "Coast/" "Vivaldi" "Phantom"))
     ;		    (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (APPLE_ITUNES((Manufacturer Manufacturer::APPLE) (BrowserBase 'APPLE_WEB_KIT) 52 "iTunes" (make-array 1 :initial-element "iTunes") nil
		   (BrowserType BrowserType::APP) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (APPLE_APPSTORE((Manufacturer Manufacturer::APPLE) (BrowserBase 'APPLE_WEB_KIT) 53 "App Store" (make-array 1 :initial-element "MacAppStore") nil
		     (BrowserType BrowserType::APP) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (ADOBE_AIR((Manufacturer Manufacturer::ADOBE) (BrowserBase 'APPLE_WEB_KIT) 1 "Adobe AIR application" (make-array 1 :initial-element "AdobeAIR") nil
		(BrowserType BrowserType::APP) (RenderingEngine RenderingEngine::WEBKIT) nil))
     (LOTUS_NOTES((Manufacturer Manufacturer::OTHER) nil 3 "Lotus Notes" (make-array 1 :initial-element "Lotus-Notes") nil
		  (BrowserType BrowserType::EMAIL_CLIENT) (RenderingEngine RenderingEngine::OTHER) "Lotus-Notes\\/(([\\d]+)\\.([\\w]+))"))
     ;(CAMINO((Manufacturer Manufacturer::OTHER) nil 5 "Camino" (make-array 1 :initial-element "Camino") nil
     ;	     (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) "Camino\\/(([0-9]+)\\.?([\\w]+)?(\\.[\\w]+)?)" ))
     (CAMINO2((Manufacturer Manufacturer::OTHER) (BrowserBase 'CAMINO) 17 "Camino 2" (make-array 1 :initial-element "Camino/2") nil
	      (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) nil))
     (FLOCK((Manufacturer Manufacturer::OTHER) nil 4 "Flock" (make-array 1 :initial-element "Flock") nil
	    (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) "Flock\\/(([0-9]+)\\.?([\\w]+)?(\\.[\\w]+)?)"))

     ;;Thunderbird email client, based on the same Gecko engine Firefox is using.
     ;(THUNDERBIRD((Manufacturer Manufacturer::MOZILLA) nil 110 "Thunderbird" (make-array 1 :initial-element "Thunderbird") nil
     ;		  (BrowserType BrowserType::EMAIL_CLIENT) (RenderingEngine RenderingEngine::GECKO) "Thunderbird\\/(([0-9]+)\\.?([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)" ))
     (THUNDERBIRD12((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'THUNDERBIRD) 185 "Thunderbird 12" (make-array 1 :initial-element "Thunderbird/12") nil
		    (BrowserType BrowserType::EMAIL_CLIENT) (RenderingEngine RenderingEngine::GECKO) nil))
     (THUNDERBIRD11((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'THUNDERBIRD) 184 "Thunderbird 12" (make-array 1 :initial-element "Thunderbird/11") nil
		    (BrowserType BrowserType::EMAIL_CLIENT) (RenderingEngine RenderingEngine::GECKO) nil))
     (THUNDERBIRD10((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'THUNDERBIRD) 183 "Thunderbird 12" (make-array 1 :initial-element "Thunderbird/10") nil
		    (BrowserType BrowserType::EMAIL_CLIENT) (RenderingEngine RenderingEngine::GECKO) nil))
     (THUNDERBIRD8((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'THUNDERBIRD) 180 "Thunderbird 12" (make-array 1 :initial-element "Thunderbird/8") nil
		    (BrowserType BrowserType::EMAIL_CLIENT) (RenderingEngine RenderingEngine::GECKO) nil))
     (THUNDERBIRD7((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'THUNDERBIRD) 170 "Thunderbird 12" (make-array 1 :initial-element "Thunderbird/7") nil
		    (BrowserType BrowserType::EMAIL_CLIENT) (RenderingEngine RenderingEngine::GECKO) nil))
     (THUNDERBIRD6((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'THUNDERBIRD) 160 "Thunderbird 12" (make-array 1 :initial-element "Thunderbird/6") nil
		    (BrowserType BrowserType::EMAIL_CLIENT) (RenderingEngine RenderingEngine::GECKO) nil))
     (THUNDERBIRD3((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'THUNDERBIRD) 130 "Thunderbird 12" (make-array 1 :initial-element "Thunderbird/3") nil
		    (BrowserType BrowserType::EMAIL_CLIENT) (RenderingEngine RenderingEngine::GECKO) nil))
     (THUNDERBIRD2((Manufacturer Manufacturer::MOZILLA) (BrowserBase 'THUNDERBIRD) 120 "Thunderbird 12" (make-array 1 :initial-element "Thunderbird/2") nil
		    (BrowserType BrowserType::EMAIL_CLIENT) (RenderingEngine RenderingEngine::GECKO) nil))

     (VIVALDI((Manufacturer Manufacturer::OTHER) nil 108338 "Vivaldi" (make-array 1 :initial-element "Vivaldi") (make-array 1 :initial-element nil)
	      (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::BLINK) "Vivaldi/(([\\d]+).([\\d]+).([\\d]+).([\\d]+))"))
     (SEAMONKEY((Manufacturer Manufacturer::OTHER) nil 15 "SeaMonkey" (make-array 1 :initial-element "SeaMonkey") nil
		(BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::GECKO) "SeaMonbbkey\\/(([0-9]+)\\.?([\\w]+)?(\\.[\\w]+)?)"))
     ;(BOT((Manufacturer Manufacturer::OTHER) nil 12 "Robot/Spider"
     ;	  (make-array 22 :initial-contents '("Googlebot" "Mediapartners-Google" "Web Preview" "bot" "Applebot" "spider" "crawler" "Feedfetcher" "Slurp" "Twiceler" "Nutch" "BecomeBot" "bingbot"
     ;					     "BingPreview" "Google Web Preview" "WordPress.com mShots" "Seznam" "facebookexternalhit" "YandexMarket" "Teoma" "ThumbSniper" "Phantom")) nil
     ;	  (BrowserType BrowserType::ROBOT) (RenderingEngine RenderingEngine::OTHER) nil))
     (BOT_MOBILE((Manufacturer Manufacturer::OTHER) (BrowserBase 'BOt) 20 "Mobil Robot/Spider" (make-array 1 :initial-element "Googlebot-Mobile") nil
		 (BrowserType BrowserType::ROBOT) (RenderingEngine RenderingEngine::OTHER) nil))
     (MOZILLA((Manufacturer Manufacturer::MOZILLA) nil 1 "Mozilla" (make-array 2 :initial-contents '("Mozilla" "Moozilla")) (make-array 1 :initial-element "ggpht.com")
	      (BrowserType BrowserType::WEB_BROWSER) (RenderingEngine RenderingEngine::OTHER) nil))
     (CFNETWORK((Manufacturer Manufacturer::OTHER) nil 6 "CFNetwork" (make-array 1 :initial-element "CFNetwork") nil
		(BrowserType BrowserType::UNKNOWN) (RenderingEngine RenderingEngine::OTHER) "CFNetwork/(([\\d]+)(?:\\.([\\d]))?(?:\\.([\\d]+))?)"))
     (EUDORA((Manufacturer Manufacturer::OTHER) nil 7 "Eudora" (make-array 2 :initial-contents '("Eudora" "EUDORA")) nil
	     (BrowserType BrowserType::EMAIL_CLIENT) (RenderingEngine RenderingEngine::OTHER) nil))
     (POCOMAIL((Manufacturer Manufacturer::OTHER) nil 8 "PocoMail" (make-array 1 :initial-element "PocoMail") nil
	       (BrowserType BrowserType::EMAIL_CLIENT) (RenderingEngine RenderingEngine::OTHER) nil))
     (THEBAT((Manufacturer Manufacturer::OTHER) nil 9 "The Bat!" (make-array 1 :initial-element "The Bat") nil
	     (BrowserType BrowserType::EMAIL_CLIENT) (RenderingEngine RenderingEngine::OTHER) nil))
     (NETFRONT((Manufacturer Manufacturer::OTHER) nil 10 "NetFront" (make-array 1 :initial-element "NetFront") nil
	       (BrowserType BrowserType::MOBILE_BROWSER) (RenderingEngine RenderingEngine::OTHER) nil))
     (EVOLUTION((Manufacturer Manufacturer::OTHER) nil 11 "Evolution" (make-array 1 :initial-element "CamelHttpStream") nil
		(BrowserType BrowserType::EMAIL_CLIENT) (RenderingEngine RenderingEngine::OTHER) nil))
     (LYNX((Manufacturer Manufacturer::OTHER) nil 13 "Lynx" (make-array 1 :initial-element "Lynx") nil
	   (BrowserType BrowserType::TEXT_BROWSER) (RenderingEngine RenderingEngine::OTHER) "Lynx\\/(([0-9]+)\\.([\\d]+)\\.?([\\w-+]+)?\\.?([\\w-+]+)?)"))
     (DOWNLOAD((Manufacturer Manufacturer::OTHER) nil 16 "Downloading Tool" (make-array 4 :initial-contents '("cURL" "wget" "ggpht.com" "Apache-HttpClient")) nil
	       (BrowserType BrowserType::TOOL) (RenderingEngine RenderingEngine::OTHER) nil))
     (UNKNOWN((Manufacturer Manufacturer::OTHER) nil 14 "Unknown" (make-array 1 :initial-element nil) nil
	      (BrowserType BrowserType::UNKNOWN) (RenderingEngine RenderingEngine::OTHER) nil))
          
     )
  ((manufacturer)
   (parent)
   (versionId)
   (namee)
   (aliases)
   (exclude)
   (browserType)
   (renderingEngine)
   (versionRegexString)

   ;;(topLevelBrowsers)
   )

  (:method initialize-instance :after ((browser Browser) &key)
    (format t "enum Browser init..."))
  (:method build-enum-initialization ('Browser)
    (format t "call method build-enum-initialization"))
  (:method parseUserAgentLowercaseString ((browser Browser) agentString)
    (if (null agentString)
	(Browser 'UNKNOWN)
	())))

