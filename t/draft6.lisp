(defpackage :json-schema/test/draft6
  (:use :cl :json-schema-test-case-helper))

(in-package :json-schema/test/draft6)

(test-cases-from-file "additionalItems")

(test-cases-from-file "additionalProperties")

(test-cases-from-file "allOf")

(test-cases-from-file "anyOf")

(test-cases-from-file "boolean_schema")

(test-cases-from-file "const")

(test-cases-from-file "contains")

(test-cases-from-file "default")

(test-cases-from-file "definitions")

(test-cases-from-file "dependencies")

(test-cases-from-file "enum")

(test-cases-from-file "exclusiveMaximum")

(test-cases-from-file "exclusiveMinimum")

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

(test-cases-from-file "multipleOf"
                      :skip (("by number" . ("4.5 is multiple of 1.5"))))

(test-cases-from-file "not")

(test-cases-from-file "oneOf")

(test-cases-from-file "pattern")

(test-cases-from-file "patternProperties")

(test-cases-from-file "properties")

(test-cases-from-file "propertyNames")

(test-cases-from-file "ref"
                      :skip (("ref overrides any sibling keywords" . ("ref valid, maxItems ignored"))))

(test-cases-from-file "refRemote")

(test-cases-from-file "required")

(test-cases-from-file "type")

(test-cases-from-file "uniqueItems")

;; optional cases

(test-cases-from-file "optional/bignum")

(test-cases-from-file "optional/non-bmp-regex")

(test-cases-from-file "optional/format/date-time"
                      :skip (("validation of date-time strings" . ("an invalid closing Z after time-zone offset"))))

(test-cases-from-file "optional/format/email")

(test-cases-from-file "optional/format/hostname")

(test-cases-from-file "optional/format/ipv4")

(test-cases-from-file "optional/format/ipv6")

(test-cases-from-file "optional/format/json-pointer")
