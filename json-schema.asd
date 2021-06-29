;;;; json-schema.asd

(defsystem json-schema
  :description "JSON schema validation"
  :author "Matt Novenstern <fisxoj@gmail.com>"
  :license "LGPL"
  :version "1.0.2"
  :pathname "src"
  :components ((:file "utils")
               (:file "parse")
               (:file "types")
               (:file "reference")
               (:file "formats")
               (:file "validators")
               (:file "json-schema"))
  :depends-on ("alexandria"
               "arrows"
               "cl-ppcre"
               "dexador"
               "function-cache"
               "local-time"
               "local-time-duration"
               "quri"
               "sanity-clause"
               "st-json"
               "str"
               "trivial-types")
  :homepage "https://fisxoj.github.io/json-schema/"
  :in-order-to ((test-op (test-op json-schema/test)))
  :long-description #.(uiop:read-file-string #P"README.rst"))

(defsystem json-schema/test-helpers
  :depends-on ("json-schema"
               "rove")
  :components ((:file "json-schema-test-case-helper")))

(defsystem json-schema/test/draft2019-09
  :depends-on ("json-schema/test-helpers")
  :pathname "t"
  :components ((:file "draft2019-09"))
  :perform (test-op (op c)
                    (declare (ignore op))
		    (uiop:symbol-call :rove :run c)))

(defsystem json-schema/test/draft7
  :depends-on ("json-schema/test-helpers")
  :pathname "t"
  :components ((:file "draft7"))
  :perform (test-op (op c)
                    (declare (ignore op))
		    (uiop:symbol-call :rove :run c)))

(defsystem json-schema/test/draft6
  :depends-on ("json-schema/test-helpers")
  :pathname "t"
  :components ((:file "draft6"))
  :perform (test-op (op c)
                    (declare (ignore op))
		    (uiop:symbol-call :rove :run c)))

(defsystem json-schema/test/draft4
  :depends-on ("json-schema/test-helpers")
  :pathname "t"
  :components ((:file "draft4"))
  :perform (test-op (op c)
                    (declare (ignore op))
		    (uiop:symbol-call :rove :run c)))

(defsystem json-schema/test/unit
  :depends-on ("json-schema"
	       "rove")
  :pathname "t"
  :components ((:file "utils")
               (:file "reference"))
  :perform (test-op (op c)
                    (declare (ignore op))
		    (uiop:symbol-call :rove :run c)))

(defsystem json-schema/test
  :in-order-to ((test-op (test-op json-schema/test)
                         (test-op json-schema/unit-tests))))
