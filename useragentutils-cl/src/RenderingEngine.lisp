(defpackage #:RenderingEngine
  (:use #:cl)
  (:export #:RenderingEngine))

(in-package #:RenderingEngine)

(defenum:defenum (RenderingEngine (:initargs (namee)))
    ((EDGE_HTML 1 ("EdgeHTML"))
     (TRIDENT 2 ("Trident"))
     (WORD("Microsoft Office Word"))
     (GECKO("Gecko"))
     (WEBKIT("WebKit"))
     (PRESTO("Presto"))
     (MOZILLA("Mozilla"))
     (KHTML("KHTML"))
     (BLINK("Blink"))
     (OTHER("Other"))

     )
    ((namee))
    ())
