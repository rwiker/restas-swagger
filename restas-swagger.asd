;;;; restas-swagger.asd

(asdf:defsystem #:restas-swagger
  :description "Simple RESTAS plugin for documenting REST APIs, using the OpenAPI (swagger) format."
  :author "Raymond Wiker <rwiker@gmail.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "restas-swagger")))
