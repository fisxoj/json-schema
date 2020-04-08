;;;; json-schema.asd

(asdf:defsystem json-schema
  :description "JSON schema validation"
  :author "Matt Novenstern <fisxoj@gmail.com>"
  :license "LGPL"
  :version "0.0.1"
  :pathname "src"
  :components ((:file "json-schema"))
  :depends-on ("sanity-clause")
  :homepage "https://fisxoj.github.io/json-schema/"
  :in-order-to ((test-op (test-op json-schema/test)))
  :long-description #.(uiop:read-file-string #P"README.rst"))


(defsystem json-schema/test
  :depends-on ("json-schema"
	       "rove")
  :pathname "t"
  :components ((:file "json-schema"))
  :perform (test-op (op c)
                    (declare (ignore op))
		    (uiop:symbol-call :rove :run c)))
