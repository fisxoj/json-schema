(defpackage json-schema/test.reference
  (:local-nicknames (:put :json-schema.reference))
  (:use :cl :rove))

(in-package :json-schema/test.reference)

(deftest test-unescape
  (ok (string= (put:unescape "#/my~0name") "#/my-name"))

  (ok (string= (put:unescape "my~1name") "my/name"))

  (ok (string= (put:unescape "my~1name~0") "my/name-"))

  (ok (string= (put:unescape "something~without~escaping") "something~without~escaping")))

(deftest test-escape
  (ok (string= (put:escape "my-cool-name") "my~0cool~0name")))
