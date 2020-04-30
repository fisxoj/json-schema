(defpackage :json-schema/test.jso-printer
  (:local-nicknames (:json :st-json))
  (:use :cl))

(in-package :json-schema/test.jso-printer)


(defmethod print-object ((object json:jso) stream)
  (format stream "#jso(~a)" (json:write-json-to-string object)))
