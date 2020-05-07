;;;; package.lisp

(defpackage #:restas-swagger/openapi
  (:use #:cl))

(defpackage #:restas-swagger
  (:use #:cl)
  (:export #:set-swagger-module-properties #:get-swagger-definition/json))

