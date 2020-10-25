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

- ``$ref`` overrides any sibling keywords

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

~~~~~~~~~~~~~
Decoding JSON
~~~~~~~~~~~~~

json-schema operates mostly on :class:`cl:hash-table` objects.  It requires them to have the ``:test`` argument set to :function:`cl:equal`, so that they work with string keys.  Further, it expects ``:true`` and ``:false`` as the boolean values and ``:null`` as the decoded Javascript ``null``.  Javascrpit arrays should be rendered as lists.  This behavior is provided behind the scenes by `st-json <https://marijnhaverbeke.nl/st-json/>`_.  The :function:`json-schema.parse:parse` function provides this functionality over strings, streams, and pathnames for you.


~~~~~~~~~~~~~~
Network access
~~~~~~~~~~~~~~

JSON Schema allows schemas to reference other documents over the network.  This library will fetch them automatically, by default.  If you don't want this to be allowed, you should set :variable:`json-schema.reference:*resolve-remote-references*` to ``nil``.  If a schema references a remote one, it will raise a :class:`json-schema.reference:fetching-not-allowed-error` instead of fetching it when fetching references is disallowed.


~~~~~~~~~~~~~~~
Reusing Schemas
~~~~~~~~~~~~~~~

Because of the nature of JSON Schema's references (location-independent references, particularly), schema documents need to be walked when loaded to discover named anchors and ids.  They also may load other schemas.

If you're reusing a large schema document repeatedly, you might want to cache the resolution context.  Unfortunately, I'm still working on this feature!
