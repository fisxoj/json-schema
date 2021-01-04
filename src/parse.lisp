(defpackage :json-schema.parse
  (:use :cl :alexandria :arrows)
  (:export
   #:parse))

(in-package :json-schema.parse)


(defun parse (input)
  (flet ((parse (input)
           (let ((*read-default-float-format* 'double-float)
                 (st-json:*decode-objects-as* :hashtable))
             (st-json:read-json input))))
    (etypecase input
      (string
       (parse input))

      (pathname
       (with-open-file (input input)
         (parse input)))

      (stream
       (parse input)))))
