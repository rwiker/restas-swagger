(in-package #:restas-swagger/utilities)

(defun valid-keyword-list-p (keys-and-values)
  (and (listp keys-and-values)
       (listp (cdr keys-and-values))
       (evenp (length keys-and-values))
       (let ((keys (loop for k in keys-and-values by 'cddr
                         collect k)))
         (when (every (lambda (k) (and (symbolp k) (keywordp k)))
                      keys)
           keys))))

(defun slot-valid (object slot)
  (and (slot-boundp object slot)
       (slot-value object slot)))

(defun slots-valid (object &rest slots)
  (every (lambda (slot)
           (slot-valid object slot))
         slots))

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

(defun expand-args (keys-and-values keys-and-classes)
  (flet ((constructor (symbol)
           (if (fboundp symbol)
             (lambda (keys-and-values)
               (apply symbol keys-and-values))
             (lambda (keys-and-values)
               (apply 'make-instance symbol keys-and-values)))))
    (loop for (key value . rest) on keys-and-values by 'cddr
          for spec = (getf keys-and-classes key)
          nconc (list key
                      (if spec
                        (cond ((symbolp spec)
                               (apply 'make-instance spec value))
                              ((and (listp spec)
                                    (eq (car spec) 'list)
                                    (symbolp (cadr spec)))
                               (mapcar (constructor (cadr spec))
                                       value))
                              ((and (listp spec)
                                    (eq (car spec) 'plist)
                                    (symbolp (cadr spec)))
                               (loop with class = (cadr spec)
                                     with constructor = (constructor class)
                                     for (key value . rest) on value by 'cddr
                                     nconc (list key (funcall constructor value))))
                              (t
                               (error "Expected symbol or (list symbol) or (plist symbol): ~a" spec)))
                        value)))))

(defun serialize-for-json-using-slots (object slots)
  (loop for slot in slots
        for value = (and (slot-boundp object slot)
                         (slot-value object slot))
        when value
        collect (cons slot (serialize-for-json value))))

(defmethod serialize-for-json ((object t))
  (identity object))

(defmethod serialize-for-json ((object function))
  (funcall object))

(defmethod serialize-for-json ((object list))
  (if (valid-keyword-list-p object)
    (loop for (key val . nil) on object by 'cddr
          collect (cons key (serialize-for-json val)))
    object))

(defmethod serialize-for-json ((object hash-table))
  (loop for key being the hash-key of object
        using (hash-value value)
        collect (cons key (serialize-for-json value))))

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

