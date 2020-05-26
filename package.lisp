;;;; package.lisp

(defpackage #:restas-swagger/utilities
  (:use #:cl)
  (:export #:valid-keyword-list-p
           #:slot-valid
           #:slots-valid
           #:extract-summary
           #:expand-args
           #:serialize-for-json
           #:serialize-for-json-using-slots
           #:as-url-component
           #:as-url
           #:plist))

(defpackage #:restas-swagger
  (:use #:cl
        #:restas-swagger/utilities)
  (:export #:register-module
           #:get-swagger-definition/json))

