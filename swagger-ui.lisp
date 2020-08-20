(in-package #:restas-swagger/swagger-ui)

(defparameter *swagger-ui-module* nil)

(restas:define-route root ("" :method :get :content-type "text/html")
  "Interactive Swagger UI."
  (merge-pathnames #p"./static/index.html"
                   (asdf/system:system-source-directory :restas-swagger)))

(restas:define-route swagger/json ("swagger.json" :method :get :content-type "application/json")
  (:example-uri)
  "Get swagger definition for current module (by *swagger-ui-module*)."
  (get-swagger-definition/json *swagger-ui-module*))
