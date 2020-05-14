;;;; restas-swagger.lisp

(in-package #:restas-swagger)

(defclass swagger-module ()
  ((info :accessor info :initarg :info :type info)
   (base-path :accessor base-path :initarg :base-path :initform "/api/v1/" :type string)
   (paths :accessor sw-paths :initform (make-hash-table :test #'equal))))

(defclass info ()
  ((title :accessor title :initarg :title)
   (description :accessor description :initarg :description)
   (terms-of-service :accessor terms-of-service :initarg :terms-of-service)
   (contact :accessor contact :initarg :contact :type contact)
   (license :accessor license :initarg :license :type license)
   (version :accessor version :initarg :version)))

(defclass contact ()
  ((name :accessor name :initarg :name)
   (url :accessor url :initarg :url)
   (email :accessor email :initarg :email)))

(defclass license ()
  ((name :accessor name :initarg :name)
   (url :accessor url :initarg :url)))

(defclass server ()
  ((url :accessor url :initarg :url)
   (description :accessor description :initarg :description)
   (variables :accessor variables :initform (make-hash-table :test #'equal))))

(defmethod serialize-for-json ((info info))
  (serialize-for-json-using-slots
   info
   '(title description version terms-of-service
           contact license)))
  
(defmethod serialize-for-json ((object swagger-module))
  `((:swagger . "2.0")
    (:info . ,(serialize-for-json (info object)))
    (:base-path . ,(serialize-for-json (base-path object)))
    (:paths . ,(loop for path being the hash-value of (sw-paths object)
                     collect (serialize-for-json path)))))

(defvar *swagger-modules*
  (make-hash-table)
  "Mapping from mounted module (packages) to swagger-module objects.")

(defmethod initialize-instance :around ((swagger-module swagger-module) &rest initargs &key &allow-other-keys)
  (apply #'call-next-method swagger-module (recursively-construct 'swagger-module initargs)))

(defun get-swagger-module (package)
  (let ((real-package (find-package package)))
    (or (gethash real-package *swagger-modules*)
        (setf (gethash real-package *swagger-modules*)
              (make-instance 'swagger-module)))))

(defmethod initialize-instance :around ((info info) &rest initargs &key &allow-other-keys)
  (apply #'call-next-method info (recursively-construct 'info initargs)))

(defun set-swagger-module-info (package keys-and-values)
  (let ((module (get-swagger-module package)))
    (if module (setf (info module)
                     (apply 'make-instance 'info keys-and-values))
      (setf (gethash (find-package package) *swagger-modules*)
            (apply 'make-instance 'swagger-module (list :info keys-and-values))))))

;;; FIXME: does not quite work... some more attention needed on paths.
(defun get-swagger-definition/json (package)
  (let ((module (get-swagger-module package)))
    (when module
      (cl-json:encode-json-to-string (serialize-for-json module)))))

#||
(get-swagger-definition/json 'sumo-surface-proto/api-v1)
||#

(defclass swagger-path ()
  ((path :accessor sw-path :initarg :path)
   (operations :accessor sw-operations :initform nil)))

(defmethod serialize-for-json ((object swagger-path))
  (flet ((ensure-absolute (path)
           (if (char= (char path 0) #\/)
             path
             (concatenate 'string "/" path))))
    (cons (ensure-absolute (sw-path object))
          (loop for (method operation . nil ) on (sw-operations object) by 'cddr
                collect (cons method (serialize-for-json operation))))))

(defclass swagger-operation ()
  ((tags :accessor sw-tags :initarg :tags :initform nil)
   (summary :accessor sw-summary :initarg :summary)
   (description :accessor sw-description :initarg :description)
   (operation-id :accessor sw-id :initarg :id)
   (consumes :accessor sw-consumes :initarg :consumes :initform nil)
   (produces :accessor sw-produces :initarg :produces :initform nil)
   (parameters :accessor sw-parameters :initarg :parameters :initform nil :type (list parameter))
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
                 operation-id consumes produces
                 parameters responses
                 deprecated security)))

(defmethod initialize-instance :around ((swagger-operation swagger-operation) &rest initargs &key &allow-other-keys)
  (apply #'call-next-method swagger-operation (recursively-construct 'swagger-operation initargs)))

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
   (required :accessor sw-required :initarg :required)))

(defmethod initialize-instance :around ((parameter parameter) &rest initargs &key &allow-other-keys)
  (apply #'call-next-method parameter (recursively-construct 'parameter initargs)))

(defmethod initialize-instance :after ((parameter parameter) &rest initargs &key &allow-other-keys)
  (when (string= (sw-in parameter) "path")
    (setf (sw-required parameter) t)))

(defmethod serialize-for-json ((parameter parameter))
  (append 
   (serialize-for-json-using-slots parameter
                                   '(name in description type format required items))))

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
        (unless (member var parameters)
          (push `(:type string :in "path" :required t) parameters)))
      (let ((sw-operation (make-swagger-operation target declarations)))
        (setf (getf (sw-operations sw-path) method) sw-operation)))))
