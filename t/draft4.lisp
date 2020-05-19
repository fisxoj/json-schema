(defpackage :json-schema/test/draft4
  (:use :cl :json-schema-test-case-helper))

(in-package :json-schema/test/draft4)
;; fixme: test optional cases, too

(test-cases-from-file "additionalItems")

(test-cases-from-file "additionalProperties")

(test-cases-from-file "allOf")

(test-cases-from-file "anyOf")

(test-cases-from-file "default")

(test-cases-from-file "definitions")

(test-cases-from-file "dependencies")

(test-cases-from-file "enum")

(test-cases-from-file "format")

(test-cases-from-file "items")

(test-cases-from-file "maximum")

(test-cases-from-file "maxItems")

(test-cases-from-file "maxLength")

(test-cases-from-file "maxProperties")

(test-cases-from-file "minimum")

(test-cases-from-file "minItems")

(test-cases-from-file "minLength")

(test-cases-from-file "minProperties")

(test-cases-from-file "multipleOf")

(test-cases-from-file "not")

(test-cases-from-file "oneOf")

(test-cases-from-file "pattern")

(test-cases-from-file "patternProperties")

(test-cases-from-file "properties")

;; (test-cases-from-file "ref")

;; (test-cases-from-file "refRemote")

(test-cases-from-file "required")

(test-cases-from-file "type")

(test-cases-from-file "uniqueItems")
