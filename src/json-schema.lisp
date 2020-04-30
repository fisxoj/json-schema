(defpackage json-schema
  (:use :cl)
  (:local-nicknames (:reference :json-schema.reference)
                    (:validators :json-schema.validators))
  (:export #:validate))

(in-package :json-schema)

(defparameter *schema-version* :draft2019-09)


(defun validate (schema data)
  (reference:with-context ()
    (reference:with-pushed-context (schema)
      (validators:validate schema data))))
