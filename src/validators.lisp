(defpackage :json-schema.validators
  (:local-nicknames (:types :json-schema.types)
                    (:utils :json-schema.utils)
                    (:reference :json-schema.reference)
                    (:json :st-json))
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
  ((property-name :initarg :property-name)
   (error-message :initarg :error-message)
   (sub-errors :initarg :sub-errors :initform nil))
  (:report (lambda (c stream)
             (format stream "Validation of property ~S failed with error:~2%~a~@[~2%Additionally:~%~{- ~a~%~}~]"
                     (slot-value c 'property-name)
                     (slot-value c 'error-message)
                     (slot-value c 'sub-errors)))))


(defmacro defvfun (name validation-field &body body)
  `(defun ,name (schema ,validation-field data)
     (macrolet ((require-type (type)
                  `(unless (validate-type nil ,type data)
                     (return-from ,',name t)))

                (condition (form error-string &rest format-args)
                  `(unless ,form
                     (error 'validation-failed-error
                            :property-name ,,(string-downcase name)
                            :error-message (format nil ,error-string
                                                   ,@format-args)))))

       ,@body)))


(defun validate-type (schema type data)
  "This is a tool for checking type validation, but isn't a validator itself.  It's used by many of the validator functions to decide wether they can have an opinion on the data being validated, but is also used by :function:`type-validator`."
  (declare (ignore schema))

  (if (listp type)
      (some (lambda (type) (types:draft2019-09 data type)) type)
      (types:draft2019-09 data type)))


(define-condition no-validator-condition ()
  ((field-name :initarg :field-name)))


(defun validate (schema data &optional (schema-version *schema-version*))
  (check-type schema-version schema-version)

  (ecase schema-version
    (:draft2019-09
     (let* ((resolved-schema (reference:ensure-resolved schema))
            (results nil))

       (json:mapjso (lambda (property value)
                      (let ((result (handler-case (draft2019-09 resolved-schema
                                                                property
                                                                value
                                                                data)

                                      (no-validator-condition (c)
                                        (warn "No validator for field ~a - skipping."
                                              (slot-value c 'field-name))
                                        nil)

                                      (validation-failed-error (error)
                                        error))))

                        (when (typep result 'condition)
                          (push result results))))
                    resolved-schema)
       results))))


(defvfun additional-properties value
  (require-type "object")

  (labels ((alist-keys (alist)
             (mapcar #'car (st-json::jso-alist alist)))

           (remove-pattern-property-keys (list)
             (if-let ((pattern-properties (json:getjso "patternProperties" schema)))

               (remove-if-not (lambda (key)
                                (some (lambda (pattern) (pattern schema pattern key))
                                      (alist-keys pattern-properties)))
                              list)
               list)))

    (cond
      ((eq value :false)
       (let* ((schema-properties (when-let ((properties (json:getjso "properties" schema)))
                                   (alist-keys properties)))

              (data-properties (alist-keys data))

              ;; pattern properties don't count as additional
              (additional-properties (remove-pattern-property-keys
                                      (set-difference data-properties
                                                      schema-properties
                                                      :test #'string=)))

              (combined-schema-properties (union schema-properties
                                                 additional-properties
                                                 :test #'string=)))

         ;; (format t "~& data-properties: ~a~% schema-properties: ~a~%different-properties: ~a~%"
         ;;         data-properties
         ;;         combined-schema-properties
         ;;         (set-difference combined-schema-properties data-properties :test #'string=))

         (condition (null
                     (set-difference data-properties
                                     combined-schema-properties
                                     :test #'string=))
                    "~a contains more properties than specified in the schema ~a"
                    data-properties combined-schema-properties)))

      ((typep value 'json:jso)
       (let* ((schema-properties (when-let ((properties (json:getjso "properties" schema)))
                                   (alist-keys properties)))

              (data-properties (alist-keys data))

              ;; pattern properties don't count as additional
              (additional-properties (remove-pattern-property-keys
                                      (set-difference data-properties
                                                      schema-properties
                                                      :test #'string=))))
         (condition (null additional-properties)
                    "The properties ~{~S~^, ~} aren't in the schema ~{~S~^, ~} schema-properties."
                    additional-properties schema-properties)))

      (t
       t))))


(defvfun const const
  (typecase const
    (json:json-null
     (condition (validate-type nil "null" data)
                "~a isn't null like the constant."
                data))

    (number
     (condition (validate-type nil "number" data)
                "~a isn't a number like constant ~d."
                data const)
     (condition (= const data)
                "~d doesn't equal constant ~d."
                data const))

    (string
     (condition (validate-type nil "string" data)
                "~a isn't a string like constant ~S."
                data const)
     (condition (string= const data)
                "~S dosen't equal constant ~S."
                data const))

    (proper-list
     (condition (validate-type nil "array" data)
                "~a isn't an array like constant ~a."
                data const)
     (condition (handler-case (progn
                                (map nil
                                     (lambda (const-el data-el)
                                       (const schema const-el data-el))
                                     const
                                     data)
                                ;; return t when every field can be checked without producing errors
                                t)
                  (validation-failed-error (error)
                    (declare (ignore error))
                    ;; Fail validation if an element isn't equal
                    nil))
                "~a doesn't equal constant ~a."
                data const))

    (json:jso
     (condition (validate-type nil "object" data)
                "~a isn't an object like constant ~a."
                data const)
     (condition (utils:object-equal-p const data)
                "~a isn't the same object as constant ~a."
                data const))

    (json:json-bool
     (condition (validate-type nil "boolean" data)
                "~a isn't a boolean like ~a."
                data const)
     (condition (eq data const)
                "~a doesn't equal constant ~a."
                data const))

    (t (error "No validator for const type ~a" (type-of const)))))


(defvfun contains contains
  (require-type "array")

  (condition (some (lambda (data) (not (validate contains data))) data)
             "~a does not contain ~a" data contains))


(defvfun exclusive-maximum maximum
  (require-type "number")

  (condition (not (>= data maximum))
             "~d must be strictly less than ~a"
             data maximum))


(defvfun exclusive-minimum minimum
  (require-type "number")

  (condition (not (<= data minimum))
             "~d must be strictly more than ~a"
             data minimum))


(defvfun type-validator type
  (condition (validate-type nil type data)
             "Value ~a is not of type ~S."
             data type))


(defvfun maximum maximum
  (require-type "number")

  (condition (<= data maximum)
             "~d must be less than or equal to ~d"
             data maximum))


(defvfun minimum minimum
  (require-type "number")

  (condition (>= data minimum)
             "~d must be greater than or equal to ~d"
             data minimum))


(defvfun properties (schema properties data)
  (require-type "object")

  (loop for (property . property-schema) in (json::jso-alist properties)
        ;; FIXME: use more generic getters here
        collecting (multiple-value-bind (property-data found-p) (json:getjso property data)

                     (let ((result (if found-p
                                       (validate property-schema property-data)
                                       t)))
                       ;; (format t "~&====~%property ~S ~a => ~a~%===~%"
                       ;;       property
                       ;;       property-data
                       ;;       result)
                       result))))


(defvfun pattern pattern
  (require-type "string")

  (condition (ppcre:scan pattern data)
             "~S didn't match pattern ~S"
             data pattern))


(defvfun required required-fields
  (require-type "object")

  (let ((missing-keys (set-difference required-fields
                                      (utils:alist-keys data)
                                      :test #'string=)))

    (condition (null missing-keys)
               "Object is missing the required keys: ~{~a~^, ~}"
               required-fields)))


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
  "additionalProperties" additional-properties
  "const" const
  "contains" contains
  "exclusiveMaximum" exclusive-maximum
  "exclusiveMinimum" exclusive-minimum
  "maximum" maximum
  "minimum" minimum
  "properties" properties
  "pattern" pattern
  "required" required
  "type" type-validator)
