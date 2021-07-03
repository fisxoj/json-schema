(defpackage :json-schema.utils
  (:local-nicknames (:json :st-json))
  (:use :cl :alexandria)
  (:export #:object-equal-p
           #:object-keys
           #:json-equal-p
           #:object-get
           #:empty-object-p

           #:schema-version
           #:object
           #:schema
           #:json-boolean
           #:json-null
           #:json-array
           #:make-empty-object
           #:json-pretty-printer))

(in-package :json-schema.utils)


(deftype schema-version ()
  '(member :draft2019-09
           :draft7
           :draft6
           :draft4
           :draft3))


(deftype object ()
  'hash-table)


(deftype json-boolean ()
  'st-json:json-bool)


(deftype json-null ()
  'st-json:json-null)


(deftype json-array ()
  'proper-list)


(deftype schema ()
  '(or object json-boolean))

(defun make-empty-object ()
  (make-hash-table :test 'equal))


(defun object-keys (object)
  (hash-table-keys object))


(defun object-get (key object &optional default)
  (declare (type string key)
           (type object object))

  (multiple-value-bind (value found-p) (gethash key object)
    (values (if found-p value default) (the boolean found-p))))


(defun empty-object-p (object)
  (zerop (hash-table-count object)))


(defun json-equal-p (thing1 thing2)
  "A generic comparison function for comparing anything that might be a json value."

  (typecase thing1
    (number
     (when (numberp thing2)
       (= thing1 thing2)))

    (string
     (when (stringp thing2)
       (string= thing1 thing2)))

    (object
     (when (typep thing2 'object)
       (object-equal-p thing1 thing2)))

    (proper-list
     (when (proper-list-p thing2)
       (and (= (length thing1) (length thing2))
            (every #'json-equal-p thing1 thing2))))

    (json-boolean
     (when (typep thing2 'json-boolean)
       (eq thing1 thing2)))

    (json-null
     (when (typep thing2 'json-null)
       (eq thing1 thing2)))))


(defun object-equal-p (object1 object2)
  (and (alexandria:set-equal (object-keys object1) (object-keys object2)
                             :test 'equal)
       (loop for key in (object-keys object1)
             for prop1 = (object-get key object1)
             for prop2 = (object-get key object2)
             unless (typecase prop1
                      (object
                       (when (typep prop2 'object)
                         (json-equal-p prop1 prop2)))

                      (t (equal prop1 prop2)))
               return nil
             finally (return t))))


(defun json-pretty-printer (stream json-object at colon)
  (declare (ignore at colon))
  (st-json:write-json json-object stream))
