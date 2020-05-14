(defpackage :json-schema/test.utils
  (:local-nicknames (:put :json-schema.utils)
                    (:json :st-json))
  (:use :cl :rove))

(in-package :json-schema/test.utils)


(deftest object-equal-p
  (testing "simple objects"
    (let ((object1 (json:read-json-from-string "{\"a\":1, \"b\":[\"r\",\"m\",\"p\"]}"))
          (object2 (json:read-json-from-string "{\"a\":1, \"b\":[\"r\",\"m\",\"p\"]}"))
          (object3 (json:read-json-from-string "{\"a\":2, \"b\":[\"r\",\"m\",\"q\"]}"))
          (object4 (json:read-json-from-string "{\"a\":1, \"b\":[\"r\",\"m\",\"q\"]}"))
          (object5 (json:read-json-from-string "{\"a\":1, \"b\":[\"r\",\"m\",\"p\"],\"c\":false}")))

      (ok (put:object-equal-p object1 object2)
          "two equal objects are equal.")

      (ng (put:object-equal-p object1 object3)
          "an object with a different value for a key is not the same.")

      (ng (put:object-equal-p object1 object4)
          "an object with a different value in an array is not the same.")

      (ng (put:object-equal-p object1 object5)
          "an object with an additional field that is otherwise the same is not the same."))))
