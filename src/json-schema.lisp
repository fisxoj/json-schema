(defpackage json-schema
  (:use :cl)
  (:local-nicknames (:types :json-schema.types)
                    (:reference :json-schema.reference))
  (:export #:validate))

(in-package :json-schema)

(defparameter *schema-version* :draft2019-09)


(defun validate (schema data &optional (schema-version *schema-version*))
  (ecase schema-version
    (:draft2019-09
     (reference:with-context ()
       (reference:with-pushed-context (schema)
         (let ((type (gethash "type" schema)))
           (if (listp type)
               (some (lambda (type) (types:draft2019-09 data type)) type)
               (types:draft2019-09 data type))))))))
