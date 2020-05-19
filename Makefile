LISP ?= sbcl --non-interactive --dynamic-space-size=4096 --eval '(push (uiop:getcwd) asdf:*central-registry*)'

test:
	# Pre-compile deps
	$(LISP) --eval "(ql:quickload :json-schema")
	$(LISP) --eval "(ql:quickload :json-schema)" \
		--eval "(asdf:load-system :json-schema/test)" \
		--eval "(uiop:quit (if (uiop:symbol-call :rove :run (asdf:find-system :openapi/test)) 0 -1))"

.PHONY: test
