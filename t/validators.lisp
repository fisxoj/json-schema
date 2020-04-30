(defpackage :json-schema/test.validators
  (:local-nicknames (:put :json-schema.validators)
                    (:reference :json-schema.reference)
                    (:json :st-json))
  (:use :cl :rove))

(in-package :json-schema/test.validators)

(deftest validate
  (testing "simple schema"
    (let ((schema (json:read-json-from-string "{
           \"properties\": {
                \"foo\": {\"type\": \"integer\"},
                \"bar\": {\"type\": \"string\"}
            }
        }")))
      (reference:with-context ()
        (reference:with-pushed-context (schema)

          (ok (put:validate schema (json:read-json-from-string "{\"foo\":4,\"bar\":\"yes\"}"))
              "validates valid data correctly.")
          (ng (put:validate schema (json:read-json-from-string "{\"foo\":4,\"bar\":10}"))
              "validates invalid data correctly.")))))

  (testing "recursive root reference"
    (let ((schema (json:read-json-from-string "{
           \"properties\": {
                \"foo\": {\"$ref\": \"#\"}
            },
           \"additionalProperties\": false
        }")))
      (reference:with-context ()
        (reference:with-pushed-context (schema)

          (ok (put:validate schema (json:read-json-from-string "{\"foo\":false}"))
              "correctly validates valid data.")
          (ng (put:validate schema (json:read-json-from-string "{\"bar\":false}"))
              "correctly validates invalid data.")))))

  (testing "encoded refs"
    (let ((schema (json:read-json-from-string "{
            \"$defs\": {
                \"tilda~field\": {\"type\": \"integer\"},
                \"slash/field\": {\"type\": \"integer\"},
                \"percent%field\": {\"type\": \"integer\"}
            },
            \"properties\": {
                \"tilda\": {\"$ref\": \"#/$defs/tilda~0field\"},
                \"slash\": {\"$ref\": \"#/$defs/slash~1field\"},
                \"percent\": {\"$ref\": \"#/$defs/percent%25field\"}
            }
         }")))
      (reference:with-context ()
        (reference:with-pushed-context (schema)

          (testing "with a slash"
            (ok (put:validate schema (json:read-json-from-string "{\"slash\":123}"))
                "is valid.")
            (ng (put:validate schema (json:read-json-from-string "{\"slash\":\"sure\"}"))
                "is invalid."))

           (testing "with a tilda"
            (ok (put:validate schema (json:read-json-from-string "{\"tilda\":123}"))
                "is valid.")
            (ng (put:validate schema (json:read-json-from-string "{\"tilda\":\"sure\"}"))
                "is invalid."))

          (testing "with a percent"
            (ok (put:validate schema (json:read-json-from-string "{\"percent\":123}"))
                "is valid.")
            (ng (put:validate schema (json:read-json-from-string "{\"percent\":\"sure\"}"))
                "is invalid.")))))))
