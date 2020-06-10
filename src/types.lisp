(defpackage :json-schema.types
  (:use #:cl
        #:alexandria)
  (:export #:draft3
           #:draft4
           #:draft6
           #:draft7
           #:draft2019-09))

(in-package :json-schema.types)

;; adapted to lisp from:
;; https://github.com/Julian/jsonschema/blob/master/jsonschema/_types.py


(defun boolean-p (value)
  (member value '(:false :true)))


(defun object-p (value)
  (typep value 'st-json:jso))


(defun any-p (value)
  (declare (ignore value))
  t)


(defun array-p (value)
  "Arrays are valid, but not strings."

  (and (proper-list-p value)
       (not (stringp value))))


(defun null-p (value)
  (eq value :null))


(defun integer-p (value)
  "JSON Schema considers anything without a fractional part an integer, ie. 1.0d0 is an integer. 🤷"
  (and (numberp value)
       (= (floor value) value)))


(defmacro def-checker (name &rest types-plist)
  `(defun ,name (value type)
     (alexandria:eswitch (type :test #'string-equal)
       ,@(loop for (type function) on types-plist by #'cddr
               collecting `(,type (,function value))))))


(def-checker draft3
  "any" any-p
  "array" arrayp
  "boolean" boolean-p
  "integer" integer-p
  "object" object-p
  "null" null-p
  "number" realp
  "string" stringp)


(def-checker draft4
  "array" array-p
  "boolean" boolean-p
  "integer" integer-p
  "object" object-p
  "null" null-p
  "number" realp
  "string" stringp)


(def-checker draft6
  "array" array-p
  "boolean" boolean-p
  "integer" integer-p
  "object" object-p
  "null" null-p
  "number" realp
  "string" stringp)


(def-checker draft7
  "array" array-p
  "boolean" boolean-p
  "integer" integer-p
  "object" object-p
  "null" null-p
  "number" realp
  "string" stringp)


(def-checker draft2019-09
  "array" array-p
  "boolean" boolean-p
  "integer" integer-p
  "object" object-p
  "null" null-p
  "number" realp
  "string" stringp)
