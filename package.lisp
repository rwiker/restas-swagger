;;;; package.lisp

(defpackage #:restas-swagger/utilities
  (:use #:cl)
  (:export #:maybe-simplify-type
           #:valid-keyword-list-p
           #:slot-valid
           #:slots-valid
           #:extract-summary
           #:recursively-construct
           #:serialize-for-json
           #:serialize-for-json-using-slots
           #:as-url-component
           #:as-url))

(defpackage #:restas-swagger/openapi
  (:use #:cl #:restas-swagger/utilities))

(defpackage #:restas-swagger/swagger
  (:use #:cl #:restas-swagger/utilities))

(defpackage #:restas-swagger
  (:use #:cl
        #:restas-swagger/utilities
        #:restas-swagger/swagger
        #:restas-swagger/openapi)
  (:export #:set-swagger-module-info
           #:set-swagger-module-properties
           #:get-swagger-definition/json))

