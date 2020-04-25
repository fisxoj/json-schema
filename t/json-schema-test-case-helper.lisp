(defpackage :json-schema-test-case-helper
  (:use :cl :alexandria :cl-arrows :rove)
  (:export #:test-cases-from-file))

(in-package :json-schema-test-case-helper)


(defun data-of (spec)
  (gethash "data" spec))


(defun valid-p (spec)
  (not (eq (gethash "valid" spec) :false)))


(defun description-of (spec)
  (gethash "description" spec))


(defun schema-of (spec)
  (gethash "schema" spec))


(defun test-cases-of (spec)
  (gethash "tests" spec))


(defun unhash (data)
  "Convert the json input of hashes/arrays into forms that are (lisp)readable for codegen."

  (typecase data
    (hash-table
     (let (alist)
       (maphash (lambda (k v) (push `(cons ,k ,(unhash v)) alist)) data)
       `(alexandria:alist-hash-table (list ,@alist) :test 'equal)))

    (list
     `(list ,@(mapcar #'unhash data)))

    (t data)))


(defun transform-spec-to-tests (spec-pathname)
  (-> spec-pathname
      read-file-into-string
      json-schema.parse:parse))


(defmacro test-cases-from-file (name)
  (let* ((version-from-package (->> *package*
                                    package-name
                                    (str:split #\/)
                                    third
                                    string-downcase))
         (pathname-directory (list :relative "JSON-Schema-Test-Suite" "tests" version-from-package))
         (test-spec-pathname (asdf:system-relative-pathname :json-schema
                                                            (make-pathname :directory pathname-directory
                                                                           :name name
                                                                           :type "json"))))

    `(deftest ,(intern (format nil "TEST-~:@(~a~)" name) *package*)
       ,@(mapcar #'spec-to-deftest
                 (transform-spec-to-tests test-spec-pathname)))))


(defun test-case-to-assertion (spec schema-gensym)
  `(,(if (valid-p spec) 'ok 'ng)
    (json-schema:validate ,schema-gensym
                          ,(unhash (data-of spec)))
    ,(description-of spec)))


(defun spec-to-deftest (spec)
  (with-gensyms (schema-gensym)

    `(testing ,(description-of spec)
       (let ((,schema-gensym ,(unhash (schema-of spec))))

         ,@(mapcar (lambda (test-spec)
                     (test-case-to-assertion test-spec schema-gensym))
                   (test-cases-of spec))))))
