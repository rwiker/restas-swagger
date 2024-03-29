;;;; restas-swagger.lisp

(in-package #:restas-swagger)

(defvar *swagger-modules*
  (make-hash-table)
  "Mapping from mounted module (packages) to swagger-module objects.")

(defun hash-table-values (hash-table)
  (loop for value being the hash-value of hash-table
        collect value))

(defclass swagger-module ()
  ((info :accessor info :initarg :info :type info)
   (base-path :accessor base-path :initarg :base-path :initform "/" :type string)
   (paths :accessor sw-paths :initform (make-hash-table :test #'equal))
   (security-definitions :accessor sw-security-definitions :initarg :security-definitions)))

(defmethod initialize-instance :around ((swagger-module swagger-module) &rest initargs &key &allow-other-keys)
  (apply #'call-next-method swagger-module (expand-args initargs '(:info info :security-definitions (plist make-security-definition)))))

(defmethod serialize-for-json ((object swagger-module))
  (let ((res
         (st-json:jso "swagger" "2.0"
                      "info" (serialize-for-json (info object))
                      "basePath" (serialize-for-json (base-path object))
                      "paths" (apply 'st-json:jso
                                     (mapcan (lambda (path)
                                               (list (serialize-for-json (concatenate 'string "/" (sw-path path)))
                                                     (serialize-for-json (sw-operations path))))
                                             (sort (hash-table-values (sw-paths object)) 'string<= :key 'sw-path))))))
    (when (and (slot-boundp object 'security-definitions)
               (slot-value object 'security-definitions))
      (setf (st-json:getjso (serialize-for-json 'security-definitions) res)
            (serialize-for-json (slot-value object 'security-definitions))))
    res))

(defclass dummy-swagger-module ()
  ((paths :accessor sw-paths :initform (make-hash-table :test #'equal))))

(defmethod serialize-for-json ((object dummy-swagger-module))
  (error "Can't serialize dummy-swagger-module. Did you forget to call register-module?"))

(defclass info ()
  ((title :accessor title :initarg :title :type string)
   (description :accessor description :initarg :description :type string)
   (terms-of-service :accessor terms-of-service :initarg :terms-of-service)
   (contact :accessor contact :initarg :contact :type contact)
   (license :accessor license :initarg :license :type license)
   (version :accessor version :initarg :version)))

(defmethod initialize-instance :around ((info info) &rest initargs &key &allow-other-keys)
  (apply #'call-next-method info (expand-args initargs '(:contact contact :license license))))

(defmethod serialize-for-json ((info info))
  (serialize-for-json-using-slots
   info
   '(title description version terms-of-service
           contact license)))

#||
(cl-json:encode-json-to-string
 (serialize-for-json (first (hash-table-values *swagger-modules*))))

(cl-json:encode-json-to-string
 (serialize-for-json
  (gethash "objects('{objectid}')"
           (sw-paths (first (hash-table-values *swagger-modules*))))))
||#

(defclass contact ()
  ((name :accessor name :initarg :name :type string)
   (url :accessor url :initarg :url :type string)
   (email :accessor email :initarg :email :type string)))

(defmethod serialize-for-json ((contact contact))
  (serialize-for-json-using-slots
   contact
   '(name url email)))

(defclass license ()
  ((name :accessor name :initarg :name :type string)
   (url :accessor url :initarg :url :type string)))

(defclass server ()
  ((url :accessor url :initarg :url :type string)
   (description :accessor description :initarg :description)
   (variables :accessor variables :initform (make-hash-table :test #'equal))))

(defclass security-definition ()
  ((type :accessor sw-type :initarg :type)
   (description :accessor sw-description :initarg :description :initform nil)))

(defmethod serialize-for-json ((security-definition security-definition))
  (serialize-for-json-using-slots
   security-definition
   '(type description)))


(defclass security-definition/basic (security-definition)
  ())

(defclass security-definition/api-key (security-definition)
  ((name :accessor sw-name :initarg :name)
   (in :accessor sw-in :initarg :in)))

(defmethod serialize-for-json ((security-definition/api-key security-definition/api-key))
  (serialize-for-json-using-slots
   security-definition/api-key
   '(type description name in)))

(defclass security-definition/oauth2 (security-definition)
  ((flow :accessor sw-flow :initarg :flow)
   (authorization-url :accessor sw-authorization-url :initarg :authorization-url)
   (token-url :accessor sw-token-url :initarg :token-url)
   (scopes :accessor sw-scopes :initarg :scopes)))


(defmethod serialize-for-json ((security-definition/oauth2 security-definition/oauth2))
  (serialize-for-json-using-slots
   security-definition/oauth2
   '(type description flow authorization-url token-url scopes)))

(defun make-security-definition (&rest keys-and-values)
  (let ((type (getf keys-and-values :type)))
    (assert type)
    (ecase type
      (:basic
       (apply 'make-instance 'security-definition/basic keys-and-values))
      (:api-key
       (apply 'make-instance 'security-definition/api-key keys-and-values))
      (:oauth2
       (apply 'make-instance 'security-definition/oauth2 (expand-args keys-and-values '()))))))

(defun get-swagger-module (package)
  (let ((real-package (find-package package)))
    (or (gethash real-package *swagger-modules*)
        (setf (gethash real-package *swagger-modules*)
              (make-instance 'dummy-swagger-module)))))

(defun register-module (package keys-and-values)
  (let ((module (apply 'make-instance 'swagger-module keys-and-values))
        (old-module (gethash (find-package package) *swagger-modules*)))
    (when old-module
      (setf (sw-paths module) (sw-paths old-module)))
    (setf (gethash (find-package package) *swagger-modules*)
          module)))

(defclass swagger-path ()
  ((path :accessor sw-path :initarg :path)
   (operations :accessor sw-operations :initform nil)))

(defmethod serialize-for-json ((object swagger-path))
  (flet ((ensure-absolute (path)
           (if (char= (char path 0) #\/)
             path
             (concatenate 'string "/" path))))
    (st-json:jso (ensure-absolute (sw-path object))
                 (loop with res = (st-json:jso)
                       with operations = (loop for (method operation) on (sw-operations object) by 'cddr
                                               collect (cons method operation))
                       with sorted-operations = (sort operations 'string<= :key 'car)
                       for (method . operation) in sorted-operations
                       do (setf (st-json:getjso (serialize-for-json method) res)
                                (serialize-for-json operation))
                       finally (return res)))))

(defclass swagger-operation ()
  ((tags :accessor sw-tags :initarg :tags :initform nil)
   (summary :accessor sw-summary :initarg :summary)
   (description :accessor sw-description :initarg :description)
   (operation-id :accessor sw-id :initarg :id)
   (consumes :accessor sw-consumes :initarg :consumes :initform nil)
   (schema :accessor sw-schema :initarg :schema :initform nil)
   (produces :accessor sw-produces :initarg :produces :initform nil)
   (parameters :accessor sw-parameters :initarg :parameters :initform nil :type list)
   (responses :accessor sw-responses :initarg :responses :initform nil)
   (deprecated :accessor sw-deprecated :initarg :deprecated :initform nil)
   (security :accessor sw-security :initarg :security :initform nil)))

(defmethod sw-summary :around ((swagger-operation swagger-operation))
  (if (slot-boundp swagger-operation 'summary)
    (call-next-method)
    (setf (slot-value swagger-operation 'summary)
          (extract-summary (documentation (sw-id swagger-operation) 'function)))))

(defmethod sw-description :around ((swagger-operation swagger-operation))
  (if (slot-boundp swagger-operation 'description)
    (call-next-method)
    (setf (slot-value swagger-operation 'description)
          (let ((doc-string (documentation (sw-id swagger-operation) 'function)))
            (if (string= (sw-summary swagger-operation) doc-string)
              nil
              doc-string)))))

(defmethod serialize-for-json ((object swagger-operation))
  ;; ensure that summary and description have values - default from
  ;; documentation string if unset.
  (sw-summary object)
  (sw-description object)
  (serialize-for-json-using-slots
   object '(tags summary description
                 operation-id consumes
                 schema produces
                 parameters responses
                 deprecated security)))

(defmethod initialize-instance :around ((swagger-operation swagger-operation) &rest initargs &key &allow-other-keys)
  (apply #'call-next-method swagger-operation (expand-args initargs '(:parameters (list parameter)))))

(defun make-swagger-operation  (id keys-and-values)
  (apply 'make-instance 'swagger-operation
         (append keys-and-values
                 (list :id id))))

(defclass swagger-security-definition ()
  ())

(defclass parameter ()
  ((name :accessor sw-name :initarg :name :type string)
   (in :accessor sw-in :initarg :in :type string)
   (type :accessor sw-type :initarg :type :type string)
   (format :accessor sw-format :initarg :format :type string)
   (items :accessor sw-items :initarg :items :type items)
   (description :accessor sw-description :initarg :description :initform nil)
   (schema :accessor sw-schema :initarg :schema :initform nil)
   (required :accessor sw-required :initarg :required)))

(defmethod initialize-instance :around ((parameter parameter) &rest initargs &key &allow-other-keys)
  (apply #'call-next-method parameter (expand-args initargs '(:items items))))

(defmethod initialize-instance :after ((parameter parameter) &rest initargs &key &allow-other-keys)
  (when (string= (string (sw-in parameter)) "path")
    (setf (sw-required parameter) t)))

(defmethod serialize-for-json ((parameter parameter))
  (append 
   (serialize-for-json-using-slots parameter
                                   '(name in description type format required items schema))))

(defclass items ()
  ((type :accessor sw-type :initarg :type :type string)
   (format :accessor sw-format :initarg :format :type string)
   (collection-format :accessor collection-format :initarg :collection-format :type string)))

(defmethod serialize-for-json ((items items))
  (serialize-for-json-using-slots items '(type format collection-format)))

#+nil
(restas:define-declaration :swagger/module :mount-module (declarations target traits)
  (print *package*)
  (break)
  (setf (gethash target *swagger-modules*) (make-swagger-module declarations)))

(restas:define-declaration :swagger/route :route (declarations target traits)
  (let ((sw-module (get-swagger-module (symbol-package target)))
        (template (gethash :template traits))
        (content-type (gethash :content-type traits))
        (method (gethash :method traits))
        (variables (gethash :variables traits))
        (parameters (getf declarations :parameters)))
    (let* ((sw-template (as-url (routes:parse-template template)))
           (sw-path (or (gethash sw-template (sw-paths sw-module))
                        (setf (gethash sw-template (sw-paths sw-module))
                              (make-instance 'swagger-path :path sw-template)))))
      (when (and content-type (not (getf declarations :produces)))
        (setf (getf declarations :produces)
              (list content-type)))
      (dolist (var variables)
        (unless (member var parameters :key (lambda (param) (getf param :name)))
          (push `(:name ,var :type string :in "path" :required t) parameters)))
      (let ((sw-operation (make-swagger-operation target declarations)))
        (setf (getf (sw-operations sw-path) method) sw-operation)))))


(defun get-swagger-definition/json (package)
  (let ((module (get-swagger-module package)))
    (when module
      (st-json:write-json-to-string (serialize-for-json module)))))

#||
(get-swagger-definition/json 'sumo-surface-proto/api-v1)
(let ((module (get-swagger-module 'sumo/api-v1)))
  (let ((path (gethash "aggregate" (sw-paths module))))
    (clrhash (sw-paths module))
    (setf (gethash "aggregate" (sw-paths module)) path)))
||#

