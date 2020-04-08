(defpackage json-schema.reference
  (:use :cl)
  (:export #:make-reference
           #:relative-reference-p
           #:escape
           #:unescape))

(in-package :json-schema.reference)

(define-condition remote-reference-error ()
  ((reference :type reference
              :initarg :reference)
   (location :initarg :location
             :initform nil))
  (:report (lambda (c stream)
             (with-slots (reference location) c
               (format stream "Encountered an absolute path (~S) in a reference~@[ (~S)~], but *resolve-remote-references* is NIL." reference location)))))

(defvar *resolve-remote-references* nil
  "Whether to download other schemas for references.  Will error if another uri is referenced in a schema and this var is set to ``nil``.")

(defun unescape (string)
  "Unescape a string to replace ~0 and ~1 with - and /."

  (with-output-to-string (out)
    (with-input-from-string (in string)

      (loop for char = (read-char in nil nil)
            for next-char = (peek-char nil in nil nil)

            while (not (null char))

            do (cond
                 ((and (char= char #\~) (char= next-char #\0))
                  (read-char in)
                  (princ #\- out))

                 ((and (char= char #\~) (char= next-char #\1))
                  (read-char in)
                  (princ #\/ out))

                 ((and (char= char #\~) (null next-char))
                  (error "String ended while reading escaped character."))

                 (t
                  (princ char out)))))))

(defun escape (string)
  (with-output-to-string (out)
    (loop for char across string
          do (cond
               ((char= char #\-)
                (princ "~0" out))

               ((char= char #\/)
                (princ "~1" out))

               (t
                (princ char out))))))

(defclass reference ()
  ((relative-path :initarg :relative-path)
   (uri :initarg :uri)))

(defun make-reference (reference-string)
  (let ((uri (quri:uri reference-string)))
    (make-instance 'reference
                   :relative-path (mapcar #'unescape
                                          (str:split #\/ (quri:uri-fragment uri)))
                   :uri (quri:render-uri (quri:copy-uri uri :fragment nil)))))

(defun relative-reference-p (reference)
  (and (null (quri:uri-scheme reference))
       (null (quri:uri-host reference))
       (null (quri:uri-path reference))))
