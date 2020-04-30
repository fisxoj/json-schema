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
     (let ((resolved-schema (reference:ensure-resolved schema)))

       (notany #'null
               (loop for (property . value) in (json::jso-alist resolved-schema)
                     for result = (handler-case (draft2019-09 resolved-schema property value data)
                                    (no-validator-condition (c)
                                      (warn "No validator for field ~a - skipping."
                                            (slot-value c 'field-name))
                                      c))

                     ;; do (format t
                     ;;            "~& validate ~a~%  property ~S~%  value ~a~%  data ~a~%  => ~a~%"
                     ;;            resolved-schema
                     ;;            property
                     ;;            value
                     ;;            data
                     ;;            result)
                     ;; do (format t "~& validate property ~S => ~a~%" property result)
                     unless (typep result 'condition)
                       collecting result))))))


(defun additional-properties (schema value data)
  (unless (typep data 'st-json:jso)
    (return-from additional-properties t))

  (if (eq value :false)
      (let ((schema-properties (mapcar #'car (st-json::jso-alist (json:getjso "properties" schema))))
            (data-properties (mapcar #'car (st-json::jso-alist data))))

        #+nil(format t "~& data-properties: ~a~% schema-properties: ~a~%different-properties: ~a~%"
                     data-properties
                     schema-properties
                     (set-difference schema-properties data-properties :test #'string=))

        (null
         (set-difference schema-properties
                         data-properties
                         :test #'string=)))
      t))


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


(defmacro def-validator (name &rest keys-plist)
  `(defun ,name (schema field value data)
     (alexandria:switch (field :test #'string-equal)
       ,@(loop for (field function) on keys-plist by #'cddr
               collecting `(,field (,function schema value data)))
       (otherwise (signal 'no-validator-condition :field-name field)))))


(def-validator draft2019-09
  "additionalProperties" additional-properties
  "type" validate-type
  "properties" properties)
