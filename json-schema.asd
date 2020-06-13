;;;; json-schema.asd

(defsystem json-schema
  :description "JSON schema validation"
  :author "Matt Novenstern <fisxoj@gmail.com>"
  :license "LGPL"
  :version "0.1.0"
  :pathname "src"
  :components ((:file "utils")
               (:file "parse")
               (:file "types")
               (:file "reference")
               (:file "formats")
               (:file "validators")
               (:file "json-schema"))
  :depends-on ("alexandria"
               "cl-arrows"
               "cl-ppcre"
               "dexador"
               "function-cache"
               "quri"
               "sanity-clause"
               "st-json"
               "str"
               "trivial-types")
  :homepage "https://fisxoj.github.io/json-schema/"
  :in-order-to ((test-op (test-op json-schema/test)))
  :long-description #.(uiop:read-file-string #P"README.rst"))


(defsystem json-schema/json-schema-test-suite
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

(defsystem json-schema/unit-tests
  :depends-on ("json-schema"
	       "rove")
  :pathname "t"
  :components ((:file "utils")
               (:file "reference"))
  :perform (test-op (op c)
                    (declare (ignore op))
		    (uiop:symbol-call :rove :run c)))

(defsystem json-schema/test
  :in-order-to ((test-op (test-op json-schema/json-schema-test-suite)
                         (test-op json-schema/unit-tests))))
