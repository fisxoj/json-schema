(defpackage :json-schema.validators
  (:local-nicknames (:types :json-schema.types)
                    (:reference :json-schema.reference)
                    (:json :st-json))
  (:use :cl :alexandria)
  (:export #:draft2019-09
           #:validate))

(in-package :json-schema.validators)

(deftype schema-version ()
  '(member :draft2019-09 :draft7 :draft6 :draft4 :draft3))


(defparameter *schema-version* :draft2019-09)


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
                                        c))))

                        ;; (format t "~& validate property ~S => ~A~%" property result)

                        (unless (typep result 'condition)
                          (push result results))))
                    resolved-schema)

       ;; (format t "~& validating data ~a against ~a => ~a~%"
       ;;         data
       ;;         resolved-schema
       ;;         (notany #'null results))

       (notany #'null results)))))


(defun additional-properties (schema value data)
  (unless (typep data 'st-json:jso)
    (return-from additional-properties t))

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

         (null
          (set-difference data-properties
                          combined-schema-properties
                          :test #'string=))))

      ((typep value 'json:jso)
       (let* ((schema-properties (when-let ((properties (json:getjso "properties" schema)))
                                   (alist-keys properties)))

              (data-properties (alist-keys data))

              ;; pattern properties don't count as additional
              (additional-properties (remove-pattern-property-keys
                                      (set-difference data-properties
                                                      schema-properties
                                                      :test #'string=))))
         (notany #'null
                 (loop for additional-property in additional-properties
                       collect (validate value (json:getjso additional-property data))))))

      (t
       t))))


(defun validate-type (schema type data)
  (declare (ignore schema))

  ;; (format t "~& validating-type ~a against ~A~%" data type)

  (if (listp type)
      (some (lambda (type) (types:draft2019-09 data type)) type)
      (types:draft2019-09 data type)))


(defun properties (schema properties data)
  (unless (typep data 'st-json:jso)
    (return-from properties t))

  (notany #'null
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
                               result)))))


(defun pattern (schema pattern data)
  (declare (ignore schema))

  (unless (typep data 'string)
    (return-from pattern t))

  (ppcre:scan pattern data))


(defmacro def-validator (name &rest keys-plist)
  `(defun ,name (schema field value data)
     (alexandria:switch (field :test #'string-equal)
       ,@(loop for (field function) on keys-plist by #'cddr
               collecting `(,field (,function schema value data)))
       (otherwise (signal 'no-validator-condition :field-name field)))))


(def-validator draft2019-09
  "additionalProperties" additional-properties
  "type" validate-type
  "properties" properties
  "pattern" pattern)
