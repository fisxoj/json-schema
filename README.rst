.. image:: https://travis-ci.org/fisxoj/json-schema.svg?branch=master
   :target: https://travis-ci.org/fisxoj/json-schema
   :alt: Travis CI status badge
.. image:: https://coveralls.io/repos/github/fisxoj/json-schema/badge.svg?branch=master
   :target: https://coveralls.io/github/fisxoj/json-schema?branch=master
   :alt: Coveralls status badge
.. image:: https://img.shields.io/badge/Contributor%20Covenant-v1.4%20adopted-ff69b4.svg
   :alt: Contributor Covenant
   :target: CODE_OF_CONDUCT.md


:Source: `https://github.com/fisxoj/json-schema <https://github.com/fisxoj/json-schema>`_
:Docs:  `https://fisxoj.github.io/json-schema/ <https://fisxoj.github.io/json-schema/>`_

json-schema is a validator for drafts 4, 6, 7, and 2019-09 of the `JSON Schema <https://json-schema.org/>`_ standard.  It is (mostly) compliant with the `common test suite <https://github.com/json-schema-org/JSON-Schema-Test-Suite>`_.  The exceptions are

**Draft 2019-09:**

- ``unevaluatedItems`` and ``unevaluatedProperties`` are unimplemented

**Drafts 4, 6, 7:**

- ``$ref`` does not override any sibling keywords

-------
Example
-------

The main entry point to the library is :function:`json-schema:validate`, which takes a schema to validate against, the data to validate against it and a draft version to use for interpreting the schema.  The default version is currently draft7.

**Validating a simple type**

Passing
::

   (json-schema:validate 3 :schema (json-schema.parse:parse "{\"type\":\"integer\"}"))
   ;; => T
   ;;    NIL

Failing (note the error messages in the second argument)
::

   (json-schema:validate 13 :schema (json-schema.parse:parse "{\"type\":\"integer\",\"maximum\":10}"))
   ;; => NIL
   ;;    ("13 must be less than or equal to 10")


**Validating an object**
::

   (setf schema (json-schema.parse:parse
              "{\"properties\":{\"foo\\nbar\":{\"type\":\"number\"},\"foo\\\"bar\":{\"type\":\"number\"},\"foo\\\\bar\":{\"type\":\"number\"},\"foo\\rbar\":{\"type\":\"number\"},\"foo\\tbar\":{\"type\":\"number\"},\"foo\\fbar\":{\"type\":\"number\"}}}"))

Passing
::

   (json-schema:validate
     (json-schema.parse:parse
       "{\"foo\\nbar\":1,\"foo\\\"bar\":1,\"foo\\\\bar\":1,\"foo\\rbar\":1,\"foo\\tbar\":1,\"foo\\fbar\":1}") :schema schema)
   ;; => T
   ;;    NIL

Failing
::

   (json-schema:validate
     (json-schema.parse:parse
       "{\"foo\\nbar\":\"1\",\"foo\\\"bar\":\"1\",\"foo\\\\bar\":\"1\",\"foo\\rbar\":\"1\",\"foo\\tbar\":\"1\",\"foo\\fbar\":\"1\"}") :schema schema)
   ;; => NIL
   ;; ("got errors validating properties
   ;;
   ;; Additionally:
   ;; - Value 1 is not of type \"number\".
   ;; - Value 1 is not of type \"number\".
   ;; - Value 1 is not of type \"number\".
   ;; - Value 1 is not of type \"number\".
   ;; - Value 1 is not of type \"number\".
   ;; - Value 1 is not of type \"number\".
   ;; ")

**Validating a document with a referenced schema**

If your data contains a top-level ``$schema`` key, you don't need to pass a schema along.  It will be fetched and validated against automatically.  This works with, for example, the `draft2019-09 meta-schema <https://json-schema.org/draft/2019-09/schema>`_.

-----------
Usage Notes
-----------

~~~~~~~~
Contexts
~~~~~~~~

A context is a reusable set of state that contains all of the fetched network resources (if your schema references external resources) and resolved ids.  By storing that all, you can reuse the validation context multiple times without fetching/resolving everything again.
::
   (ql:quickload '(trivial-benchmark json-schema))

   (defvar *schema* (json-schema.parse:parse #P"~/Downloads/schema"))

   ;; schema is the json-schema meta schema document from:
   ;; https://json-schema.org/specification-links.html#draft-2019-09-formerly-known-as-draft-8

   (defvar *context*
     (json-schema:make-context
      *schema*
      :draft2019-09))

   ;;; Cached

   (let ((data (json-schema.parse:parse "{\"type\": \"string\"}")))
     (trivial-benchmark:with-timing (1000)
       (json-schema:validate data
                             :context *context*)))

   ;; -                SAMPLES  TOTAL      MINIMUM  MAXIMUM   MEDIAN    AVERAGE    DEVIATION
   ;; REAL-TIME        1000     0.826      0        0.022     0.001     0.000826   0.000797
   ;; RUN-TIME         1000     0.826      0        0.022     0.001     0.000826   0.0008
   ;; USER-RUN-TIME    1000     0.781011   0        0.020644  0.000745  0.000781   0.000665
   ;; SYSTEM-RUN-TIME  1000     0.049933   0        0.000986  0         0.00005    0.000184
   ;; PAGE-FAULTS      1000     0          0        0         0         0          0.0
   ;; GC-RUN-TIME      1000     0.02       0        0.02      0         0.00002    0.000632
   ;; BYTES-CONSED     1000     213753664  195344   228976    228032    213753.66  16221.591
   ;; EVAL-CALLS       1000     0          0        0         0         0          0.0


   ;;; Uncached

   (let ((data (json-schema.parse:parse "{\"type\": \"string\"}")))
     (trivial-benchmark:with-timing (1000)
       (json-schema:validate data
                             :schema *schema*
                             :schema-version :draft2019-09)))

   ;; -                SAMPLES  TOTAL      MINIMUM   MAXIMUM   MEDIAN    AVERAGE   DEVIATION
   ;; REAL-TIME        1000     203.185    0.148     1.471     0.185     0.203185  0.112807
   ;; RUN-TIME         1000     9.25       0.006     0.04      0.009     0.00925   0.002294
   ;; USER-RUN-TIME    1000     8.145081   0.003368  0.039067  0.008105  0.008145  0.002317
   ;; SYSTEM-RUN-TIME  1000     1.107377   0         0.004927  0.000994  0.001107  0.000967
   ;; PAGE-FAULTS      1000     0          0         0         0         0         0.0
   ;; GC-RUN-TIME      1000     0.08       0         0.03      0         0.00008   0.001464
   ;; BYTES-CONSED     1000     719780512  707728    751424    718160    719780.5  11026.181
   ;; EVAL-CALLS       1000     0          0         0         0         0         0.0


So, for this trivial example, the cached version is around a 245x speedup!  Note, though, that json-schema evaluates these things lazily, so not every reference is necessarily resolved when the context is created.  They are mutable, though, and will build up state as they go.

Thank you to `Raymond Wiker <https://github.com/rwiker>`_ for contributing the initial implementation.

~~~~~~~~~~~~~
Decoding JSON
~~~~~~~~~~~~~

json-schema operates mostly on :class:`cl:hash-table` objects.  It requires them to have the ``:test`` argument set to :function:`cl:equal`, so that they work with string keys.  Further, it expects ``:true`` and ``:false`` as the boolean values and ``:null`` as the decoded Javascript ``null``.  Javascrpit arrays should be rendered as lists.  This behavior is provided behind the scenes by `st-json <https://marijnhaverbeke.nl/st-json/>`_.  The :function:`json-schema.parse:parse` function provides this functionality over strings, streams, and pathnames for you.


~~~~~~~~~~~~~~
Network access
~~~~~~~~~~~~~~

JSON Schema allows schemas to reference other documents over the network.  This library will fetch them automatically, by default.  If you don't want this to be allowed, you should set :variable:`json-schema.reference:*resolve-remote-references*` to ``nil``.  If a schema references a remote one, it will raise a :class:`json-schema.reference:fetching-not-allowed-error` instead of fetching it when fetching references is disallowed.
