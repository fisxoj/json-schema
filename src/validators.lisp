(defpackage :json-schema.validators
  (:local-nicknames (:formats :json-schema.formats)
                    (:types :json-schema.types)
                    (:utils :json-schema.utils)
                    (:reference :json-schema.reference))
  (:use :cl :alexandria)
  (:export #:draft2019-09
           #:validate
           #:validation-failed-error))

(in-package :json-schema.validators)

(deftype schema-version ()
  '(member :draft2019-09
           :draft7
           :draft6
           :draft4
           :draft3))


(defparameter *schema-version* :draft2019-09)


(define-condition validation-failed-error (error)
  ((error-message :initarg :error-message)
   (property-name :initarg :property-name :initform nil)
   (sub-errors :initarg :sub-errors :initform nil))
  (:report (lambda (c stream)
             (format stream "~a~@[~2%Additionally:~%~{- ~a~%~}~]"
                     (slot-value c 'error-message)
                     (slot-value c 'sub-errors)))))


(defmacro defvfun (name validation-field &body body)
  (flet ((property-name (name)
           ;; When properties shadow symbols in the CL package, they get named {property}-validator, so clean that off for prettier names
           (let ((string (string-downcase name)))
             (if (str:ends-with-p "-validator" string)
                 (subseq string 0 (- (length string) #.(length "-VALIDATOR")))
                 string))))

    `(defun ,name (schema ,validation-field data)
       (macrolet ((require-type (type)
                    `(unless (validate-type nil ,type data)
                       (return-from ,',name t)))

                  (condition (form error-string &rest format-args)
                    `(unless ,form
                       (error 'validation-failed-error
                              :property-name ,,(property-name name)
                              :error-message (format nil ,error-string
                                                     ,@format-args))))

                  (sub-errors (errors error-string &rest format-args)
                    (once-only (errors)
                      `(when ,errors
                         (error 'validation-failed-error
                                :sub-errors ,errors
                                :property-name ,,(property-name name)
                                :error-message (format nil ,error-string
                                                       ,@format-args))))))

         ,@body))))


(defun validate-type (schema type data)
  "This is a tool for checking type validation, but isn't a validator itself.  It's used by many of the validator functions to decide wether they can have an opinion on the data being validated, but is also used by :function:`type-validator`."
  (declare (ignore schema))

  (if (listp type)
      (some (lambda (type) (types:draft2019-09 data type)) type)
      (types:draft2019-09 data type)))


(define-condition no-validator-condition ()
  ((field-name :initarg :field-name)))


(defun validate (schema data &optional (schema-version *schema-version*) ignore-id)
  (check-type schema-version schema-version)

  (let ((*schema-version* schema-version))
    (ecase schema-version
      (:draft2019-09
       (cond
         ((typep schema 'utils:json-boolean)
          (if (eq schema :true)
              nil
              (list
               (make-instance 'validation-failed-error
                              :property-name ""
                              :error-message "Schema :false is always false."))))

         ((utils:empty-object-p schema)
          nil)

         ((typep schema 'json-schema.utils:object)
          (loop for property in (utils:object-keys schema)
                for value = (utils:object-get property schema)
                appending (handler-case (progn
                                          (draft2019-09 schema
                                                        property
                                                        value
                                                        data)
                                          nil)

                            (no-validator-condition (c)
                              (warn "No validator for field ~S - skipping."
                                    (slot-value c 'field-name))
                              nil)

                            (validation-failed-error (error)
                              (list error)))))))

      (:draft7
       (cond
         ((typep schema 'utils:json-boolean)
          (if (eq schema :true)
              nil
              (list
               (make-instance 'validation-failed-error
                              :property-name ""
                              :error-message "Schema :false is always false."))))

         ((utils:empty-object-p schema)
          nil)

         ((and (utils:object-get "$id" schema) (not ignore-id))
          (reference:with-pushed-id ((utils:object-get "$id" schema))
            (validate schema data schema-version t)))

         ((typep schema 'json-schema.utils:object)
          (loop for property in (utils:object-keys schema)
                for value = (utils:object-get property schema)
                appending (handler-case (progn
                                          (draft7 schema
                                                  property
                                                  value
                                                  data)
                                          nil)

                            (no-validator-condition (c)
                              (warn "No validator for field ~S - skipping."
                                    (slot-value c 'field-name))
                              nil)

                            (validation-failed-error (error)
                              (list error))))))))))


;;; Validation functions for individaul properties


(defun noop (schema property data)
  "This exists to say we have taken care of a property, but we should do nothing with it.  Likely because this property is actually handled by other things.  ``else`` and ``then`` are handled by :function:`if-validator`, &c."

  (declare (ignore schema property data)))


(defvfun $ref reference
  (reference:with-resolved-ref (schema resolved-schema)
    (sub-errors (validate resolved-schema data)
                "Error validating referred schema at ~S."
                reference)))


(defvfun additional-items additional-items
  (require-type "array")

  (when (validate-type nil "object" (utils:object-get "items" schema (utils:make-empty-object)))
    (return-from additional-items))

  (let ((items-length (length (utils:object-get "items" schema))))
    ;; There are only additional items if there are more than the items schema
    ;; mentions
    (when (> (length data) items-length)
      (sub-errors (loop for item in (subseq data items-length)
                        appending (validate additional-items item))
                  "Errors validating additional items against ~a."
                  additional-items))))


(defvfun additional-properties value
  (require-type "object")

  (labels ((remove-pattern-property-keys (list)
             (if-let ((pattern-properties (utils:object-get "patternProperties" schema)))

               (remove-if (lambda (key)
                            (some (lambda (pattern) (ppcre:scan pattern key))
                                  (utils:object-keys pattern-properties)))
                          list)
               list)))

    (cond
      ((eq value :false)
       (let* ((schema-properties (when-let ((properties (utils:object-get "properties" schema)))
                                   (utils:object-keys properties)))

              (data-properties (utils:object-keys data))

              ;; pattern properties don't count as additional
              (additional-properties (remove-pattern-property-keys
                                      (set-difference data-properties
                                                      schema-properties
                                                      :test #'string=))))

         (condition (null additional-properties)
                    "~S contains more properties (~S) than specified in the schema ~S"
                    data-properties additional-properties schema-properties)))

      ((typep value 'utils:object)
       (let* ((schema-properties (when-let ((properties (utils:object-get "properties" schema)))
                                   (utils:object-keys properties)))

              (data-properties (utils:object-keys data))

              ;; pattern properties don't count as additional
              (additional-properties (remove-pattern-property-keys
                                      (set-difference data-properties
                                                      schema-properties
                                                      :test #'string=)))
              (errors (loop for property in additional-properties
                            appending (validate value (utils:object-get property data)))))
         (sub-errors errors
                     "There were errors validating additional properties."))))))


(defvfun all-of sub-schemas
  (loop for sub-schema in sub-schemas
        appending (validate sub-schema data) into errors
        finally (sub-errors errors
                   "~a didn't satisfy all schemas in ~{~/json-schema.utils:json-pretty-printer/~^, ~}"
                   data
                   sub-schemas)))


(defvfun any-of sub-schemas

  (let ((errors (loop for sub-schema in sub-schemas
                      for errors = (validate sub-schema data)

                      when (null errors)
                        return nil

                      collect errors into all-errors

                      finally (return all-errors))))

    (sub-errors errors
                "~a isn't valid for any of the given schemas."
                data)))


(defvfun const const
  (condition (utils:json-equal-p data const)
             "~a is not equal to constant ~a."
             data const))


(defvfun contains contains
  (require-type "array")

  (condition (some (lambda (data) (not (validate contains data))) data)
             "~a does not contain ~a."
             data contains))


(defvfun description description
  (condition (stringp description)
             "Description must be a string."))


(defvfun dependent-required dependencies
  (require-type "object")

  (let ((failed-dependencies (remove-if #'null
                       (loop for key in (utils:object-keys dependencies)
                             when (nth-value 1 (utils:object-get key data))
                               unless (every (lambda (dependency-key)
                                               (nth-value 1 (utils:object-get dependency-key data)))
                                             (utils:object-get key dependencies))
                               ;; when the key is found in the data
                                 collecting (make-instance 'validation-failed-error
                               :property-name "dependencies"
                               :error-message (format nil "Field ~S depends on fields ~S, but some were missing."
                                                      key
                                                      (utils:object-get key dependencies)))))))

    (sub-errors failed-dependencies
                "There were failed dependencies.")))


(defvfun dependencies dependencies
  (require-type "object")

  (flet ((check-dependency (key dependency)
           (etypecase dependency
             (utils:json-array
              (unless (every (lambda (dependency-key)
                               (nth-value 1 (utils:object-get dependency-key data)))
                             dependency)
                (make-instance 'validation-failed-error
                               :property-name "dependencies"
                               :error-message (format nil "Field ~S depends on fields ~S, but some were missing."
                                                      key
                                                      (utils:object-get key dependencies)))))

             ;; A subschema... 😭
             ;; maybe an object, maybe true, false
             (t
              (when-let ((validation-errors (validate dependency data *schema-version*)))
                (make-instance 'validation-failed-error
                               :property-name "dependencies"
                               :error-message (format nil "Field ~S depends on the schema ~/json-schema.utils:json-pretty-printer/ being valid, but it wasn't."
                                                      key
                                                      dependency)))))))

    (let ((failed-dependencies
            (remove-if #'null
                       (loop for key in (utils:object-keys dependencies)
                             when (nth-value 1 (utils:object-get key data))
                               ;; when the key is found in the data
                               collecting (check-dependency key (utils:object-get key dependencies))))))

      (sub-errors failed-dependencies
                  "There were failed dependencies."))))


(defvfun enum members
  (condition (member data members :test #'utils:json-equal-p)
             "~a isn't one of ~{~a~^, ~}."
             data members))


(defvfun exclusive-maximum maximum
  (require-type "number")

  (condition (not (>= data maximum))
             "~d must be strictly less than ~a."
             data maximum))


(defvfun exclusive-minimum minimum
  (require-type "number")

  (condition (not (<= data minimum))
             "~d must be strictly more than ~a."
             data minimum))

(defvfun format-validator type
  (require-type "string")

  (flet ((validate ()
           (ecase *schema-version*
             (:draft2019-09 (formats:draft2019-09 data type))
             (:draft7       (formats:draft7 data type))
             (:draft6       (formats:draft6 data type))
             (:draft4       (formats:draft4 data type))
             (:draft3       (formats:draft3 data type)))))

    (condition (validate)
               "~a is not of format ~a."
               data type)))


(defvfun if-validator condition-schema
  (if (null (validate condition-schema data))
      (when-let ((then-schema (utils:object-get "then" schema)))
        (sub-errors (validate then-schema data)
                    "Errors occurred validating then clause."))
      (when-let ((else-schema (utils:object-get "else" schema)))
        (sub-errors (validate else-schema data)
                    "Errors occurred validating else clause."))))


(defvfun items items
  (require-type "array")

  (if (typep items 'utils:json-array)
      ;; There are schemas in the items property
      (sub-errors (loop for sub-schema in items
                        for item in data
                        appending (validate sub-schema item))
                  "Errors occurred validating items of an array.")
      ;; There is one schema for every item in the array
      (sub-errors (loop for item in data
                        appending (validate items item))
                  "Errors occurred validating items against ~/json-schema.utils:json-pretty-printer/."
                  items)))


(defvfun type-validator type
  (condition (typep type '(or utils:json-array string))
             "~S is an invalid type specifier."
             type)
  (condition (validate-type nil type data)
             "Value ~a is not of type ~S."
             data type))


(defvfun maximum maximum
  (require-type "number")

  (condition (<= data maximum)
             "~d must be less than or equal to ~d"
             data maximum))


(defvfun max-length length
  (require-type "string")

  (condition (>= length (length data))
             "String ~S must be at most ~d characters long."
             data length))


(defvfun max-items length
  (require-type "array")

  (condition (>= length (length data))
             "Array ~/json-schema.utils:json-pretty-printer/ must be at most ~d items long."
             data
             length))


(defvfun minimum minimum
  (require-type "number")

  (condition (>= data minimum)
             "~d must be greater than or equal to ~d"
             data minimum))


(defvfun min-items length
  (require-type "array")

  (condition (<= length (length data))
             "Array ~a must be at least ~d items long."
             data length))

(defvfun min-length length
  (require-type "string")

  (condition (<= length (length data))
             "String ~S must be at least ~d characters long."
             data length))


(defvfun min-properties count
  (require-type "object")

  (condition (>= (length (utils:object-keys data)) count)
             "~a must have at least ~d properties."
             data count))


(defvfun max-properties count
  (require-type "object")

  (condition (<= (length (utils:object-keys data)) count)
             "~a must have at most ~d properties."
             data count))


(defvfun multiple-of divisor
  (require-type "number")

  (when (zerop divisor)
    (return-from multiple-of))

  (etypecase divisor
    (integer
     (condition (zerop (mod (the number data) divisor))
                "~d is not a multiple of ~d."
                data divisor))

    (real
     (condition (= (truncate data divisor) (/ data divisor))
                "~d is not a multiple of ~d."
                data divisor))))


(defvfun not-validator sub-schema
  (condition (not (null (validate sub-schema data)))
             "~a should not be valid under ~/json-schema.utils:json-pretty-printer/."
             data sub-schema))


(defvfun one-of sub-schemas
  (let ((errors-for-schema (loop for sub-schema in sub-schemas
                                 collecting (validate sub-schema data))))

    (condition (some #'null errors-for-schema)
               "~a was not valid under any given schema."
               data)

    (condition (= 1 (length (remove-if-not #'null errors-for-schema)))
               "~a was valid for more than one given schema."
               data)))


(defvfun pattern-properties patterns
  (require-type "object")

  (flet ((test-key (key)
           (loop for pattern-property in (utils:object-keys patterns)
                 for property-schema = (utils:object-get pattern-property patterns)
                 for property-data = (utils:object-get key data)

                 when (ppcre:scan pattern-property key)
                   appending (handler-case (validate property-schema property-data)
                               (validation-failed-error (error)
                                 error)))))

    (let* ((errors (loop for data-property in (utils:object-keys data)
                         appending (test-key data-property))))

      (sub-errors errors
                  "got errors validating properties"))))


(defvfun properties properties
  (require-type "object")

  (let ((errors (loop for property in (utils:object-keys properties)
                      for property-schema = (utils:object-get property properties)
                      for (property-data found-p) = (multiple-value-list (utils:object-get property data))

                      when found-p
                        appending (validate property-schema property-data))))

    (sub-errors errors
                "got errors validating properties")))


(defvfun property-names names-schema
  (require-type "object")

  (sub-errors (loop for property in (utils:object-keys data)
                    appending (validate names-schema property))
              "Errors validating propertyNames."))


(defvfun pattern pattern
  (require-type "string")

  (condition (ppcre:scan pattern data)
             "~S didn't match pattern ~S"
             data pattern))


(defvfun required required-fields
  (require-type "object")

  (let ((missing-keys (set-difference required-fields
                                      (utils:object-keys data)
                                      :test #'string=)))

    (condition (null missing-keys)
               "Object is missing the required keys: ~{~a~^, ~}"
               required-fields)))


(defvfun unique-items unique
  (require-type "array")

  (when (eq unique :true)
    (condition (= (length data)
                  (length (remove-duplicates data :test 'utils:json-equal-p)))
               "Not all items in ~{~a~^, ~} are unique."
               data)))


;;; Validators for properties in different drafts of jsonschema


(defmacro def-validator (name &rest keys-plist)
  `(defun ,name (schema field value data)
     (alexandria:switch (field :test #'string-equal)
       ,@(loop for (field function) on keys-plist by #'cddr
               for error = (handler-case `(,field (,function schema value data))
                             (validation-failed-error (error)
                               error))
               when error
                 collecting error)
       (otherwise (signal 'no-validator-condition :field-name field)))))


(def-validator draft2019-09
  "$defs" noop
  "$ref" $ref
  "additionalItems" additional-items
  "additionalProperties" additional-properties
  "allOf" all-of
  "anyOf" any-of
  "const" const
  "contains" contains
  "dependentRequired" dependent-required
  "else" noop
  "enum" enum
  "exclusiveMaximum" exclusive-maximum
  "exclusiveMinimum" exclusive-minimum
  "if" if-validator
  "items" items
  "maximum" maximum
  "maxItems" max-items
  "maxLength" max-length
  "maxProperties" max-properties
  "minimum" minimum
  "minItems" min-items
  "minLength" min-length
  "minProperties" min-properties
  "multipleOf" multiple-of
  "not" not-validator
  "oneOf" one-of
  "patternProperties" pattern-properties
  "properties" properties
  "propertyNames" property-names
  "pattern" pattern
  "required" required
  "then" noop
  "type" type-validator
  "uniqueItems" unique-items)


(def-validator draft7
  "$defs" noop
  "$id" noop
  "$ref" $ref
  "$schema" noop
  "additionalItems" additional-items
  "additionalProperties" additional-properties
  "allOf" all-of
  "anyOf" any-of
  "const" const
  "contains" contains
  "description" description
  "dependencies" dependencies
  "else" noop
  "enum" enum
  "exclusiveMaximum" exclusive-maximum
  "exclusiveMinimum" exclusive-minimum
  "format" format-validator
  "if" if-validator
  "items" items
  "maximum" maximum
  "maxItems" max-items
  "maxLength" max-length
  "maxProperties" max-properties
  "minimum" minimum
  "minItems" min-items
  "minLength" min-length
  "minProperties" min-properties
  "multipleOf" multiple-of
  "not" not-validator
  "oneOf" one-of
  "patternProperties" pattern-properties
  "properties" properties
  "propertyNames" property-names
  "pattern" pattern
  "required" required
  "then" noop
  "type" type-validator
  "uniqueItems" unique-items)
