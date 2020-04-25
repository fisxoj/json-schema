(defpackage json-schema.reference
  (:use :cl :alexandria :cl-arrows)
  (:export #:make-reference
           #:with-context
           #:push-context
           #:relative-reference-p
           #:escape
           #:unescape
           #:with-pushed-context))

(in-package :json-schema.reference)

;;; NOTE: inside this file, `ref` refers to the property of a json object, and `reference` is the structure object we use internally for bookkeeping.


(defvar *context* nil
  "The lookup context for references.")


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


(defstruct context
  (current-uri nil :type (trivial-types:proper-list string))
  (references (make-hash-table :test 'equal) :type hash-table))


(defun default-id-fun (schema)
  (if (not (null schema))
      (gethash "$id" schema)
      ""))


(defmacro with-context (() &body body)
  `(let ((*context* (make-context)))
     ,@body))


(defun push-context (schema &optional (id-fun #'default-id-fun))
  (push (funcall id-fun schema) (context-current-uri *context*))
  (setf (gethash (funcall id-fun schema) (context-references *context*)) schema))


(defun pop-context ()
  (pop (context-current-uri *context*)))


(defmacro with-pushed-context ((schema &optional (id-fun ''default-id-fun)) &body body)
  (once-only (schema)
    `(unwind-protect
          (progn
            (push-context ,schema ,id-fun)
            ,@body)
       (pop-context))))


(defun unescape (string)
  "Unescape a string to replace ~0 and ~1 with ~ and /."

  (with-output-to-string (out)
    (with-input-from-string (in string)

      (loop for char = (read-char in nil nil)
            for next-char = (peek-char nil in nil nil)

            while (not (null char))

            do (cond
                 ((and (char= char #\~) (char= next-char #\0))
                  (read-char in)
                  (princ #\~ out))

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
               ((char= char #\~)
                (princ "~0" out))

               ((char= char #\/)
                (princ "~1" out))

               (t
                (princ char out))))))


(defclass reference ()
  ((relative-path :type (trivial-types:proper-list string)
                  :initarg :relative-path
                  :accessor relative-path-of)
   (uri :type string
        :initarg :uri
        :accessor uri-of)))


(defun reference-eq (reference1 reference2)
  (declare (type reference reference1 reference2))

  (and (string= (uri-of reference1) (uri-of reference2))
       (= (length (relative-path-of reference1))
          (length (relative-path-of reference2)))
       (every #'= (relative-path-of reference1) (relative-path-of reference2))))


(defun make-reference (reference-string)
  (let ((uri (quri:uri reference-string)))
    (make-instance 'reference
                   :relative-path (mapcar (compose #'quri:url-decode #'unescape)
                                          (str:split #\/ (quri:uri-fragment uri)))
                   :uri (quri:render-uri (quri:copy-uri uri
                                                        :fragment nil
                                                        :query nil)))))


(defun relative-reference-p (reference)
  (and (null (quri:uri-scheme reference))
       (null (quri:uri-host reference))
       (null (quri:uri-path reference))))


(defun fetch-reference (uri)
  (-> uri
      (dex:get :read-timeout 10
               :connect-timeout 10)
      (jojo:parse :as :hash-table)))


(defun get-ref (spec)
  (gethash "$ref" spec))


(defun ref-p (spec)
  "A spec is a reference if it has only one key which is ``$ref``."

  (and (= 1 (hash-table-count spec))
       (nth-value 1 (gethash "$ref" spec))))


(defun ensure-resolved (spec)
  "Resolve a reference if it exists, otherwise return the object.  Returns a second value indicating if there was a reference."

  (if (ref-p spec)
      (values (resolve spec) t)
      (values spec nil)))


(defun resolve (ref)
  ;; (let ((reference (make-reference (get-ref ref))))

  ;;   (if (relative-reference-p reference)
  ;;       ))
  )
