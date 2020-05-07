;;;; restas-swagger.lisp

(in-package #:restas-swagger)

(defclass swagger-module ()
  ((title :accessor sw-title :initarg :title)
   (description :accessor sw-description :initarg :description :initform nil)
   (version :accessor sw-version :initarg :version)
   (terms-of-service :accessor sw-terms-of-service :initarg :terms-of-service :initform nil)
   (contact :accessor sw-contact :initarg :contact :initform nil)
   (license :accessor sw-license :initarg :license :initform nil)
   (paths :accessor sw-paths :initform (make-hash-table :test #'equal))))

(defmethod serialize-for-json ((object t))
  (identity object))

(defmethod serialize-for-json ((object list))
  (if (valid-keyword-list-p object)
    (loop for (key val . nil) on object by 'cddr
          collect (cons key (serialize-for-json val)))
    (mapcar 'serialize-for-json object)))

(defmethod serialize-for-json ((object swagger-module))
  `((:swagger . "2.0")
    (:info .
     ,(loop for slot in '(title description version terms-of-service
                                contact license)
            for value = (and (slot-boundp object slot)
                             (slot-value object slot))
            when value
            collect (cons slot (serialize-for-json value))))
    (:paths . ,(loop for path being the hash-value of (sw-paths object)
                     collect (serialize-for-json path)))))

(defvar *swagger-modules*
  (make-hash-table)
  "Mapping from mounted module (packages) to swagger-module objects.")

(defun valid-keyword-list-p (keys-and-values)
  (and (evenp (length keys-and-values))
       (every (lambda (k) (and (symbolp k) (keywordp k)))
              (loop for k in keys-and-values by 'cddr
                    collect k))))

(defun slot-valid (object slot)
  (and (slot-boundp object slot)
       (slot-value object slot)))

(defun slots-valid (object &rest slots)
  (every (lambda (slot)
           (slot-valid object slot))
         slots))

(defun make-swagger-module (keys-and-values)
  (assert (valid-keyword-list-p keys-and-values))
  (apply 'make-instance 'swagger-module keys-and-values))

(defmethod shared-initialize :after ((object swagger-module) slot-names &rest initargs &key &allow-other-keys)
  (assert (and (slots-valid object 'title 'version))))

(defun get-swagger-module (package)
  (let ((real-package (find-package package)))
    (or (gethash real-package *swagger-modules*)
        (setf (gethash real-package *swagger-modules*)
              (make-swagger-module (list :title (package-name real-package)
                                         :description (documentation real-package t)
                                         :version "0.0"))))))

(defun set-swagger-module-properties (package keys-and-values)
  (assert (valid-keyword-list-p keys-and-values))
  (let ((module (get-swagger-module package)))
    (loop for (key value . nil) on keys-and-values by 'cddr
          do (setf (slot-value module (intern (symbol-name key) '#:restas-swagger))
                   value))))

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
   (summary :accessor sw-summary :initarg :summary :initform nil)
   (description :accessor sw-description :initarg :description :initform nil)
   (operation-id :accessor sw-id :initarg :id)
   (consumes :accessor sw-consumes :initarg :consumes :initform nil)
   (produces :accessor sw-produces :initarg :produces :initform nil)
   (parameters :accessor sw-parameters :initarg :parameters :initform nil)
   (responses :accessor sw-responses :initarg :responses :initform nil)
   (deprecated :accessor sw-deprecated :initarg :deprecated :initform nil)
   (security :accessor sw-security :initarg :security :initform nil)))

(defmethod serialize-for-json ((object swagger-operation))
  (loop for slot in '(tags summary description
                           operation-id consumes produces
                           parameters responses
                           deprecated security)
        for value = (when (slot-boundp object slot)
                      (slot-value object slot))
        when value
        collect (cons slot (serialize-for-json value))))


(defun make-swagger-operation  (id keys-and-values)
  (assert (valid-keyword-list-p keys-and-values))
  (apply 'make-instance 'swagger-operation `(:id ,id ,@keys-and-values)))

(defclass swagger-security-definition ()
  ())

(defmethod as-url-component ((template string))
  template)

(defmethod as-url-component ((template symbol))
  (format nil "{~a}" (cl-json:lisp-to-camel-case (symbol-name template))))

(defmethod as-url-component ((template routes:concat-template))
  (apply 'concatenate 'string (mapcar 'as-url-component (routes:template-data template))))

(defmethod as-url-component ((template routes:variable-template))
  (as-url-component (routes:template-data template)))

(defmethod as-url-component ((template routes:wildcard-template))
  "*")

(defmethod as-url-component ((template list))
  (mapcar 'as-url-component template))

(defun as-url (what)
  (puri:render-uri (make-instance 'puri:uri
                                  :path (reduce (lambda (a b) (concatenate 'string a "/" b))
                                                (as-url-component what)))
                   nil))

#+nil
(restas:define-declaration :swagger/module :mount-module (declarations target traits)
  (print *package*)
  (break)
  (setf (gethash target *swagger-modules*) (make-swagger-module declarations)))

(defun extract-summary (doc-string)
  (when doc-string
    (with-input-from-string (s doc-string)
      (let ((line (read-line s nil)))
        (let ((pos (position #\. line)))
          (if pos
            (subseq line 0 (1+ pos))
            (if (read-line s nil)
              (concatenate 'string line "...")
              (concatenate 'string line "."))))))))

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
        (unless (getf parameters var)
          (setf (getf parameters var) `(:type string :in "path"))))
      (setf (getf declarations :parameters) parameters)
      (unless (getf declarations :summary)
        (setf (getf declarations :summary)
              (extract-summary (documentation target 'function))))
      (unless (getf declarations :description)
        (setf (getf declarations :description)
              (documentation target 'function)))
      (let ((sw-operation (make-swagger-operation target declarations)))
        (setf (getf (sw-operations sw-path) method) sw-operation)))))
