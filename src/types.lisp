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
  (member value '(:false t)))


(defun object-p (value)
  (hash-table-p value))


(defun any-p (value)
  (declare (ignore value))
  t)


(defun array-p (value)
  "Arrays are valid, but not strings."

  ;; Jonathan only decodes arrays into lists, so, we have to work with that...
  (and (not (null value))
       (or (arrayp value)
           (proper-list-p value))
       (not (stringp value))))


(defmacro def-checker (name &rest types-plist)
  `(defun ,name (value type)
     (alexandria:eswitch (type :test #'string-equal)
       ,@(loop for (type function) on types-plist by #'cddr
               collecting `(,type (,function value))))))


(def-checker draft3
  "any" any-p
  "array" arrayp
  "boolean" boolean-p
  "integer" integerp
  "object" object-p
  "null" null
  "number" realp
  "string" stringp)


(def-checker draft4
  "array" array-p
  "boolean" boolean-p
  "integer" integerp
  "object" object-p
  "null" null
  "number" realp
  "string" stringp)


(def-checker draft6
  "array" array-p
  "boolean" boolean-p
  "integer" integerp
  "object" object-p
  "null" null
  "number" realp
  "string" stringp)


(def-checker draft7
  "array" array-p
  "boolean" boolean-p
  "integer" integerp
  "object" object-p
  "null" null
  "number" realp
  "string" stringp)

(def-checker draft2019-09
  "array" array-p
  "boolean" boolean-p
  "integer" integerp
  "object" object-p
  "null" null
  "number" realp
  "string" stringp)
