(defpackage :json-schema-test-case-helper
  (:use :cl :alexandria :cl-arrows :rove)
  (:export #:test-cases-from-file))

(in-package :json-schema-test-case-helper)

(defclass test-case ()
  ((description :type string
                :initarg :description
                :accessor description-of
                :documentation "Short description of the test case.")
   (data :type hash-table
         :initarg :data
         :accessor data-of
         :documentation "The data to validate against the schema for the test-suite.")
   (valid :type boolean
          :initarg :valid
          :accessor valid-p
          :documentation "Whether we expect validation to succeed for this test case.")))


(defclass test-suite ()
  ((description :type string
                :initarg :description
                :accessor description-of
                :documentation "Short description of the test suite.")
   (schema :type t ;; FIXME: once this is defined json-schema:schema
           :initarg :schema
           :accessor schema-of
           :documentation "The schema to validate the test cases in this suite against.")
   (test-cases :type (trivial-types:proper-list test-case)
               :initarg :test-cases
               :accessor test-cases-of
               :documentation "The test cases for this suite.")))


(defun transform-spec-to-tests (spec-pathname)
  (labels ((make-test-case (data)
             (make-instance 'test-case
                            :valid (gethash "valid" data)
                            :data (gethash "data" data)
                            :description (gethash "description" data)))
           (make-suite (data)
             (make-instance 'test-suite
                            :schema (gethash "schema" data)
                            :description (gethash "description" data)
                            :test-cases (mapcar #'make-test-case (gethash "tests" data)))))

    (-<> (read-file-into-string spec-pathname)
         (jojo:parse :as :hash-table)
         (mapcar #'make-suite <>))))


(defmacro test-cases-from-file (name)
  (let* ((version-from-package (string-downcase (third (str:split #\/ (package-name *package*)))))
         (pathname-directory (list :relative "JSON-Schema-Test-Suite" "tests" version-from-package))
         (suites (transform-spec-to-tests (asdf:system-relative-pathname :json-schema (make-pathname :directory pathname-directory
                                                                                                     :name name
                                                                                                     :type "json")))))
    `(deftest ,(intern (format nil "TEST-~:@(~a~)" name) *package*)
       ,@(mapcar #'spec-to-deftest suites))))


(defun test-case-to-assertion (spec schema)
  `(,(if (valid-p spec) 'ok 'ng)
    (json-schema:validate ,schema ',(data-of spec))
    ,(description-of spec)))


(defun spec-to-deftest (spec)
  `(testing ,(description-of spec)
     ,@(mapcar (lambda (test-spec) (test-case-to-assertion test-spec (schema-of spec)))
               (test-cases-of spec))))
