(defpackage :json-schema.utils
  (:local-nicknames (:json :st-json))
  (:use :cl)
  (:export #:object-equal-p
           #:object-keys))

(in-package :json-schema.utils)


(defun object-keys (alist)
  (mapcar #'car (st-json::jso-alist alist)))


(defun object-equal-p (object1 object2)
  (and (alexandria:set-equal (object-keys object1) (object-keys object2) :test 'equal)
       (loop for key in (object-keys object1)
             for prop1 = (json:getjso key object1)
             for prop2 = (json:getjso key object2)
             unless (typecase prop1
                      (st-json:jso
                       (when (typep prop2 'st-json:jso)
                         (object-equal-p prop1 prop2)))

                      (t (equal prop1 prop2)))
               return nil
             finally (return t))))
