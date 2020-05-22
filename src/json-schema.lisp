(defpackage json-schema
  (:use :cl :alexandria)
  (:local-nicknames (:reference :json-schema.reference)
                    (:validators :json-schema.validators))
  (:import-from :json-schema.validators
                :schema-version)
  (:export #:validate
           #:*schema-version*
           #:schema-version))

(in-package :json-schema)

(defparameter *schema-version* :draft2019-09)


(defun validate (schema data &key (schema-version *schema-version*))
  (reference:with-context ()
    (reference:with-pushed-context (schema)
      (if-let ((errors (validators:validate schema data schema-version)))
        (values nil errors)
        (values t nil)))))
