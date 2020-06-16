(defpackage :json-schema-test-case-helper
  (:use :cl :alexandria :cl-arrows :rove)
  (:local-nicknames (:utils :json-schema.utils))
  (:export #:test-cases-from-file))

(in-package :json-schema-test-case-helper)

(defvar *skips* nil
  "For keeping track of tests to skip.")


(defun data-of (spec)
  (utils:object-get "data" spec))


(defun valid-p (spec)
  (eq (utils:object-get "valid" spec) :true))


(defun description-of (spec)
  (utils:object-get "description" spec))


(defun schema-of (spec)
  (utils:object-get "schema" spec))


(defun test-cases-of (spec)
  (utils:object-get "tests" spec))


(defun unhash (data)
  (typecase data
    (utils:object
     `(json-schema.parse:parse ,(st-json:write-json-to-string data)))

    (list
     `(list ,@(mapcar #'unhash data)))

    (t data)))


(defmacro test-cases-from-file (name &key skip)
  (let* ((*skips* skip)
         (version-from-package (->> *package*
                                    package-name
                                    (str:split #\/)
                                    third
                                    string-downcase))
         (pathname-directory (list :relative "JSON-Schema-Test-Suite" "tests" version-from-package))
         (test-spec-pathname (asdf:system-relative-pathname :json-schema
                                                            (merge-pathnames (pathname name)
                                                                             (make-pathname :directory pathname-directory
                                                                                            :type "json")))))

    `(deftest ,(intern (format nil "TEST-~:@(~a~)" name) *package*)
       (let ((json-schema:*schema-version* ,(make-keyword (string-upcase version-from-package))))
         ,@(mapcar #'spec-to-deftest
                   (json-schema.parse:parse test-spec-pathname))))))


(defun check-test-skip (suite-spec assertion-spec)
  (flet ((aget (key alist &optional default)
           (or (cdr (assoc key alist :test #'string=))
               default)))

    (when *skips*
      (or (eq *skips* t) ;; skip all
          (let ((group-skips (aget (description-of suite-spec) *skips*)))
            (or (eq group-skips t)
                (find (description-of assertion-spec) group-skips
                      :test #'string=)))))))


(defun test-case-to-assertion (suite-spec assertion-spec schema-gensym)
  (if (check-test-skip suite-spec assertion-spec)
      `(skip ,(description-of assertion-spec))

      `(,(if (valid-p assertion-spec) 'ok 'ng)
        (json-schema:validate ,schema-gensym
                              ,(unhash (data-of assertion-spec)))
        ,(description-of assertion-spec))))


(defun spec-to-deftest (spec)
  (with-gensyms (schema-gensym)

    `(testing ,(description-of spec)
       (let ((,schema-gensym ,(unhash (schema-of spec))))

         ,@(mapcar (lambda (test-spec)
                     (test-case-to-assertion spec test-spec schema-gensym))
                   (test-cases-of spec))))))
