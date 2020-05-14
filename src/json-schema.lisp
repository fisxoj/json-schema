(defpackage json-schema
  (:use :cl :alexandria)
  (:local-nicknames (:reference :json-schema.reference)
                    (:validators :json-schema.validators))
  (:export #:validate))

(in-package :json-schema)

(defparameter *schema-version* :draft2019-09)


(defun validate (schema data)
  (reference:with-context ()
    (reference:with-pushed-context (schema)
      (if-let ((errors (validators:validate schema data)))
        (values nil errors)
        (values t nil)))))
