(defpackage :json-schema/test/draft2019-09
  (:use :cl :json-schema-test-case-helper))

(in-package :json-schema/test/draft2019-09)

(test-cases-from-file "additionalItems")

(test-cases-from-file "additionalProperties")

(test-cases-from-file "allOf")

(test-cases-from-file "anchor")

(test-cases-from-file "anyOf")

(test-cases-from-file "boolean_schema")

(test-cases-from-file "const")

(test-cases-from-file "contains")

(test-cases-from-file "default")

(test-cases-from-file "defs")

(test-cases-from-file "dependentRequired")

(test-cases-from-file "dependentSchemas")

(test-cases-from-file "enum")

(test-cases-from-file "exclusiveMaximum")

(test-cases-from-file "exclusiveMinimum")

(test-cases-from-file "format")

(test-cases-from-file "if-then-else")

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

(test-cases-from-file "propertyNames")

;; (test-cases-from-file "ref")

;; (test-cases-from-file "refRemote")

(test-cases-from-file "required")

(test-cases-from-file "type")

(test-cases-from-file "unevaluatedItems")

(test-cases-from-file "unevaluatedProperties")

(test-cases-from-file "uniqueItems")
