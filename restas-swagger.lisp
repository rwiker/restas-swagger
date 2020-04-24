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

(defvar *swagger-modules*
  (make-hash-table)
  "Mapping from mounted module (packages) to swagger-module objects.")

#||
(loop for v being the hash-values of *swagger-modules*
      collect (cl-json:encode-json-to-string
               `((:swagger . "2.0")
                 (:info .
                  ,(loop for slot in '(title description version terms-of-service
                                             contact license)
                         for value = (and (slot-boundp v slot)
                                          (slot-value v slot))
                         when value
                         collect (cons slot value)))
                 (:paths . ,(sw-paths v)))))
||#

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

(defclass swagger-path ()
  ((path :accessor sw-path :initarg :path)
   (operations :accessor sw-operations :initform nil)))

(defclass swagger-operation ()
  ((tags :accessor sw-tags :initarg :tags :initform nil)
   (summary :accessor sw-summary :initarg :summary :initform nil)
   (description :accessor sw-description :initarg :description :initform nil)
   (id :accessor sw-id :initarg :id)
   (consumes :accessor sw-consumes :initarg :consumes :initform nil)
   (produces :accessor sw-produces :initarg :produces :initform nil)
   (parameters :accessor sw-parameters :initarg :parameters :initform nil)
   (responses :accessor sw-responses :initarg :responses :initform nil)
   (deprecated :accessor sw-deprecated :initarg :deprecated :initform nil)
   (security :accessor sw-security :initarg :security :initform nil)))

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
  (with-input-from-string (s doc-string)
    (let ((line (read-line s nil)))
      (let ((pos (position #\. line)))
        (if pos
          (subseq line 0 (1+ pos))
          (if (read-line s nil)
            (concatenate 'string line "...")
            (concatenate 'string line ".")))))))

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
        (unless (getf parameters var )
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
