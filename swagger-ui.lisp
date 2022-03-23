(in-package #:restas-swagger/swagger-ui)

(defparameter *swagger-ui-module* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *root*
    (with-open-file (in (merge-pathnames #p"./static/index.html"
                                         (asdf/system:system-source-directory :restas-swagger))
                        :direction :input)
      (with-output-to-string (out)
        (loop for line = (read-line in nil)
              while line
              do (write-line line out))))))

(restas:define-route root ("" :method :get :content-type "text/html")
  "Interactive Swagger UI."
  *root*)

(restas:define-route swagger/json ("swagger.json" :method :get :content-type "application/json")
  "Get swagger definition for current module (by *swagger-ui-module*)."
  (get-swagger-definition/json *swagger-ui-module*))
