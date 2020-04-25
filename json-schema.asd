;;;; json-schema.asd

(defsystem json-schema
  :description "JSON schema validation"
  :author "Matt Novenstern <fisxoj@gmail.com>"
  :license "LGPL"
  :version "0.0.1"
  :pathname "src"
  :components ((:file "json-schema"))
  :depends-on ("dexador"
               "function-cache"
               "jonathan"
               "sanity-clause"
               "trivial-types")
  :homepage "https://fisxoj.github.io/json-schema/"
  :in-order-to ((test-op (test-op json-schema/test)))
  :long-description #.(uiop:read-file-string #P"README.rst"))


(defsystem json-schema/test
  :depends-on ("json-schema"
	       "rove")
  :pathname "t"
  :components ((:file "json-schema-test-case-helper")
               (:file "draft2019-09")
               (:file "draft7")
               (:file "draft6"))
  :perform (test-op (op c)
                    (declare (ignore op))
		    (uiop:symbol-call :rove :run c)))
