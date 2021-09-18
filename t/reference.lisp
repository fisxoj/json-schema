(defpackage :json-schema/test.reference
  (:local-nicknames (:put :json-schema.reference)
                    (:utils :json-schema.utils)
                    (:parse :json-schema.parse))
  (:use :cl :rove))

(in-package :json-schema/test.reference)

(defmacro with-context ((&key uri-stack references named-references (schema-version :draft7)) &body body)
  (let ((references (or references (make-hash-table :test 'equal)))
        (named-references (or named-references (make-hash-table :test 'equal))))
    `(let ((json-schema.reference::*context* (json-schema.reference::%make-context
                                              :uri-stack ,uri-stack
                                              :references ,references
                                              :named-references ,named-references
                                              :schema-version ,schema-version
                                              :root-schema (make-hash-table :test 'equal))))
       ,@body)))


(deftest test-unescape
  (ok (string= (put::unescape "#/my-name") "#/my-name"))

  (ok (string= (put::unescape "my~1name") "my/name"))

  (ok (string= (put::unescape "my~1name~0") "my/name~")))


(deftest make-reference
  (with-context (:uri-stack '(""))
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

  ;; https://github.com/fisxoj/json-schema/pull/16
  (testing "a reference inside a referenced document"
    (with-context (:uri-stack '("https://somewhere.com/schemas/department.json"))
      (ok (string= (put::uri-of (put::make-reference "employee.json"))
                   "https://somewhere.com/schemas/employee.json")
          "inherits the relative path of the document."))))


(deftest test-escape
  (ok (string= (put::escape "my~cool~name") "my~0cool~0name")))


(deftest test-context
  (testing "an uncomplicated context"
    (let ((simple-ref (parse:parse
                       "{\"components\": {\"key\": 4, \"another\": {\"$ref\": \"#/components/key\"}}}")))
      (put:with-context ((put:make-context simple-ref :draft7))
        (let ((resolved (put::resolve (utils:object-get "another" (utils:object-get "components" simple-ref)))))
          (ok (not (null resolved))
              "can resolve a reference.")

          (ok (= resolved 4)
              "can resolve a simple relative reference to the correct value.")))))

  (testing "with an encoded path component"
    (let ((simple-ref (parse:parse
                       "{\"components\": {\"~key\": 4, \"another\": {\"$ref\": \"#/components/~0key\"}}}")))
      (put:with-context ((put:make-context simple-ref :draft7))
        (let ((resolved (put::resolve (utils:object-get "another" (utils:object-get "components" simple-ref)))))
          (ok (not (null resolved))
              "can resolve a reference.")

          (ok (= resolved 4)
              "can resolve a simple relative reference to the correct value.")))))

  (testing "encoded ref names"
    (let ((document (parse:parse "{\"$defs\": {
                \"tilda~field\": {\"type\": \"integer\"},
                \"slash/field\": {\"type\": \"integer\"},
                \"percent%field\": {\"type\": \"integer\"}
            },
            \"properties\": {
                \"tilda\": {\"$ref\": \"#/$defs/tilda~0field\"},
                \"slash\": {\"$ref\": \"#/$defs/slash~1field\"},
                \"percent\": {\"$ref\": \"#/$defs/percent%25field\"}}}")))
      (put:with-context ((put:make-context document :draft7))
        (ok (string= (utils:object-get "type"
                                       (put:resolve (utils:object-get "slash" (utils:object-get "properties" document))))
                     "integer")
            "can fetch a ref with an encoded slash in it.")

        (ok (string= (utils:object-get "type"
                                       (put:resolve (utils:object-get "tilda" (utils:object-get "properties" document))))
                     "integer")
            "can fetch a ref with an encoded tilda in it.")

        (ok (string= (utils:object-get "type"
                                       (put:resolve (utils:object-get "percent" (utils:object-get "properties" document))))
                     "integer")
            "can fetch a ref with an encoded percent sign in it.")))))
