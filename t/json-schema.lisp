(defpackage json-schema/test
  (:use :cl :rove))

(in-package :json-schema/test)

(deftest test-json-schema
  (ok (eq 1 1) "testing works"))
