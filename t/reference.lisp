(defpackage :json-schema/test.reference
  (:local-nicknames (:put :json-schema.reference)
                    (:json :st-json))
  (:use :cl :rove))

(in-package :json-schema/test.reference)


(deftest test-unescape
  (ok (string= (put:unescape "#/my-name") "#/my-name"))

  (ok (string= (put:unescape "my~1name") "my/name"))

  (ok (string= (put:unescape "my~1name~0") "my/name~")))


(deftest make-reference
  (let ((relative-reference (put::make-reference "#/something/2/3/another"))
        (absolute-only-reference (put::make-reference "https://example.com/potato-schema.json"))
        (another-reference (put::make-reference "https://example.com/potato-schema.json#/somewhere/in/the/document")))

    (testing "a relative reference"
      (ok (string= ""
                   (put::uri-of relative-reference))
          "has no uri.")
      (ok (= (length (put::relative-path-of relative-reference)) 4)
          "has a list of components for a relative path.")

      (let ((parts (put::relative-path-of relative-reference)))
        (ok (and (stringp (first parts))
                 (integerp (second parts))
                 (integerp (third parts))
                 (stringp (fourth parts)))
            "the relativite path components are strings or integers.")))

    (testing "an absolute reference"
      (ok (string= "https://example.com/potato-schema.json"
                   (put::uri-of absolute-only-reference))
          "has a uri.")
      (ok (null (put::relative-path-of absolute-only-reference))
          "has a an empty list of components for a relative path."))

    (testing "a reference with both components"
      (ok (string= "https://example.com/potato-schema.json"
                   (put::uri-of another-reference))
          "has a uri.")
      (ok (= (length (put::relative-path-of relative-reference)) 4)
          "has a list of components for a relative path."))))


(deftest test-escape
  (ok (string= (put:escape "my~cool~name") "my~0cool~0name")))


(deftest test-context
  (testing "an uncomplicated context"
    (let ((simple-ref (json:read-json-from-string
                       "{\"components\": {\"key\": 4, \"another\": {\"$ref\": \"#/components/key\"}}}")))
      (put:with-context ()
        (put:with-pushed-context (simple-ref))
        (let ((resolved (put::resolve (st-json:getjso* "components.another" simple-ref))))
          (ok (not (null resolved))
              "can resolve a reference.")

          (ok (= resolved 4)
              "can resolve a simple relative reference to the correct value.")))))

  (testing "an with an encoded path component"
    (let ((simple-ref (json:read-json-from-string
                       "{\"components\": {\"~key\": 4, \"another\": {\"$ref\": \"#/components/~0key\"}}}")))
      (put:with-context ()
        (put:with-pushed-context (simple-ref))
        (let ((resolved (put::resolve (st-json:getjso* "components.another" simple-ref))))
          (ok (not (null resolved))
              "can resolve a reference.")

          (ok (= resolved 4)
              "can resolve a simple relative reference to the correct value.")))))

  (testing "encoded ref names"
    (let ((document (json:read-json-from-string "{\"$defs\": {
                \"tilda~field\": {\"type\": \"integer\"},
                \"slash/field\": {\"type\": \"integer\"},
                \"percent%field\": {\"type\": \"integer\"}
            },
            \"properties\": {
                \"tilda\": {\"$ref\": \"#/$defs/tilda~0field\"},
                \"slash\": {\"$ref\": \"#/$defs/slash~1field\"},
                \"percent\": {\"$ref\": \"#/$defs/percent%25field\"}}}")))
      (put:with-context ()
        (put:with-pushed-context (document)

          (ok (string= (json:getjso "type"
                                    (put:resolve (json:getjso* "properties.slash" document)))
                       "integer")
              "can fetch a ref with an encoded slash in it.")

          (ok (string= (json:getjso "type"
                                    (put:resolve (json:getjso* "properties.tilda" document)))
                       "integer")
              "can fetch a ref with an encoded tilda in it.")

          (ok (string= (json:getjso "type"
                                    (put:resolve (json:getjso* "properties.percent" document)))
                       "integer")
              "can fetch a ref with an encoded percent sign in it."))))))


(deftest get-subspec-by-ref
  (testing "get-subspec-by-ref"
    (let ((document (json:read-json-from-string "{\"toplevel\":[1,2,3,4,5],\"another\":{\"something\": null,\"potato\":[{\"name\":\"yes\"},{\"name\":\"no\"}]}}")))

      (ok (= (put:get-subspec-by-ref document "/toplevel/2") 3)
          "correctly indexes into arrays.")

      (ok (eq (put:get-subspec-by-ref document "/another/something") :null)
          "correctly indexes into nested objects.")

      (ok (eq (put:get-subspec-by-ref document nil) document)
          "correctly finds the root element."))))
