(defpackage :json-schema.parse
  (:use :cl :alexandria :cl-arrows)
  (:export
   #:parse))

(in-package :json-schema.parse)


;; (defun parse (input &key (buffer-size 16384))
;;   (let ((jojo:*empty-array-value* (make-array 0))
;;         (jojo:*false-value* :false))
;;     (->
;;      (etypecase input
;;        (string
;;         input)

;;        (pathname
;;         (read-file-into-string input :buffer-size buffer-size))

;;        (stream
;;         (read-stream-content-into-string input :buffer-size buffer-size)))

;;      (jojo:parse :as :hash-table))))

(defun parse (input)
  (flet ((parse (input)
           (st-json:read-json input)))
    (etypecase input
      (string
       (parse input))

      (pathname
       (with-open-file (input input)
         (parse input)))

      (stream
       (parse input)))))
