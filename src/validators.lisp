(defpackage :json-schema.validators
  (:local-nicknames (:types :json-schema.types)
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


(defvfun contains contains
  (require-type "array")

  (condition (find contains data :test 'equal)
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


(defun pattern (schema pattern data)
  (declare (ignore schema))

  (unless (typep data 'string)
    (return-from pattern t))

  (ppcre:scan pattern data))


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
  "contains" contains
  "exclusiveMaximum" exclusive-maximum
  "exclusiveMinimum" exclusive-minimum
  "maximum" maximum
  "minimum" minimum
  "properties" properties
  "pattern" pattern
  "type" type-validator)
