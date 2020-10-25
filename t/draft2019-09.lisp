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

(test-cases-from-file "defs"
                      :skip (("invalid definition" . ("invalid definition schema"))))

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

(test-cases-from-file "multipleOf"
                      :skip (("by number" . ("4.5 is multiple of 1.5"))))

(test-cases-from-file "not")

(test-cases-from-file "oneOf")

(test-cases-from-file "pattern")

(test-cases-from-file "patternProperties")

(test-cases-from-file "properties")

(test-cases-from-file "propertyNames")

(test-cases-from-file "ref")

(test-cases-from-file "refRemote"
                      :skip (("base URI change - change folder" . t)))

(test-cases-from-file "required")

(test-cases-from-file "type")

(test-cases-from-file "unevaluatedItems"
                      :skip t)

(test-cases-from-file "unevaluatedProperties"
                      :skip t)

(test-cases-from-file "uniqueItems")

(test-cases-from-file "optional/bignum")

(test-cases-from-file "optional/format/date")

(test-cases-from-file "optional/format/date-time")

(test-cases-from-file "optional/format/duration")

(test-cases-from-file "optional/format/email")

(test-cases-from-file "optional/format/hostname")

(test-cases-from-file "optional/format/idn-email"
                      :skip t)

(test-cases-from-file "optional/format/idn-hostname"
                      :skip t)

(test-cases-from-file "optional/format/ipv4")

(test-cases-from-file "optional/format/ipv6")

(test-cases-from-file "optional/format/iri"
                      :skip t)

(test-cases-from-file "optional/format/iri-reference"
                      :skip t)

(test-cases-from-file "optional/format/json-pointer")

(test-cases-from-file "optional/format/regex")

(test-cases-from-file "optional/format/json-pointer"
                      :skip t)

(test-cases-from-file "optional/format/time")

(test-cases-from-file "optional/format/uri")

(test-cases-from-file "optional/format/uri-reference")

(test-cases-from-file "optional/format/uri-template"
                      :skip t)
