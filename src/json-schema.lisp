(defpackage json-schema
  (:use :cl :alexandria)
  (:local-nicknames (:reference :json-schema.reference)
                    (:validators :json-schema.validators))
  (:shadowing-import-from :json-schema.validators
                          :schema-version)
  (:export #:validate
           #:*schema-version*
           #:schema-version))

(in-package :json-schema)


(defparameter *schema-version* :draft7)


(defun validate (schema data &key (schema-version *schema-version*) (pretty-errors-p t))
  (reference:with-context ((ecase schema-version
                             (:draft2019-09
                              'reference::draft2019-09-id-fun)
                             ((or :draft7 :draft6)
                              'reference::default-id-fun)
                             (:draft4
                              'reference::draft4-id-fun)))
    (reference:with-pushed-context (schema)
      (if-let ((errors (validators:validate schema data schema-version)))
        (values nil (mapcar (if pretty-errors-p #'princ-to-string #'identity) errors))
        (values t nil)))))
