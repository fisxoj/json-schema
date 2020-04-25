(defpackage :json-schema.parse
  (:use :cl :alexandria :cl-arrows)
  (:export
   #:parse))

(in-package :json-schema.parse)


(defun parse (input)
  (let ((jojo:*empty-array-value* (make-array 0))
        (jojo:*false-value* :false))
    (->
     (etypecase input
       (string
        input)

       (pathname
        (read-file-into-string input))

       (stream
        (read-stream-content-into-string input)))

     (jojo:parse :as :hash-table))))
