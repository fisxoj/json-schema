(defpackage json-schema
  (:use :cl :alexandria)
  (:local-nicknames (:reference :json-schema.reference)
                    (:validators :json-schema.validators))
  (:shadowing-import-from :json-schema.utils
                          :schema-version)
  (:import-from #:json-schema.reference
                #:make-context)
  (:export #:validate
           #:make-context
           #:*schema-version*
           #:schema-version))

(in-package :json-schema)


(defparameter *schema-version* :draft7)


(defun validate (data &key (schema-version *schema-version*) (pretty-errors-p t) schema context)
  "The primary validation function for json-schema.  Takes data: which can be a simple value or an object as a hash table, and then optionally accepts a schema (if the data doesn't contain a top-level ``$schema`` key), schema version and pretty-errors-p deterimines whether the second return value is exception objects or strings of the rendered errors (strings by default).

The third return value is a :class:`json-schema.reference::context`, which contains all of the state stored in processing a schema including caching network resources and all of the resolved ids."
  (assert (not (and schema context)) nil "You should only pass one of ")

  (let* ((schema (or schema
                     (and context (reference:context-schema-version context))
                     (reference:fetch-schema (json-schema.utils:object-get "$schema" data))))
         (context (or context (reference:make-context
                               (or schema (reference:fetch-schema (json-schema.utils:object-get "$schema" data)))
                               schema-version))))
    (reference:with-context (context)
      (if-let ((errors (validators:validate
                        (reference:context-root-schema context)
                        data
                        (reference:context-schema-version context))))
        (values nil (mapcar (if pretty-errors-p #'princ-to-string #'identity) errors) context)
        (values t nil context)))))
