(defpackage json-schema/test.reference
  (:local-nicknames (:put :json-schema.reference))
  (:use :cl :rove))

(in-package :json-schema/test.reference)


(deftest test-unescape
  (ok (string= (put:unescape "#/my-name") "#/my-name"))

  (ok (string= (put:unescape "my~1name") "my/name"))

  (ok (string= (put:unescape "my~1name~0") "my/name~")))


(deftest test-escape
  (ok (string= (put:escape "my~cool~name") "my~0cool~0name")))


(deftest test-context
  (let ((simple-ref (jojo:parse "{\"components\": {\"key\": 4, \"another\": {\"$ref\": \"#/components/key\"}}" :as :hash-table)))
    (put:with-context ()
      (put:with-pushed-context (simple-ref))
      (ok (= (put::resolve (gethash "another" (gethash "components" simple-ref))) 4)
          "can resolve a simple relative reference."))))
