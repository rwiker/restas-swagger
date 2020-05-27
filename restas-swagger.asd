;;;; restas-swagger.asd

(asdf:defsystem #:restas-swagger
  :description "Simple RESTAS plugin for documenting REST APIs, using the OpenAPI (swagger) format."
  :author "Raymond Wiker <rwiker@gmail.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (swank restas cl-json)
  :components ((:file "package")
               (:file "utilities")
               (:file "restas-swagger")))
