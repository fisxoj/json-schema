(defpackage json-schema.reference
  (:use :cl :alexandria :cl-arrows)
  (:local-nicknames (:json :st-json))
  (:export #:make-reference
           #:with-context
           #:push-context
           #:relative-reference-p
           #:escape
           #:unescape
           #:with-pushed-context
           #:get-subspec-by-ref
           #:ensure-resolved
           #:resolve))

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
  (if (not (eq :null schema))
      (json:getjso "$id" schema)
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


(defun make-relative-path-list (relative-path-string)
  (mapcar (lambda (element)
            (cond
              ((every (lambda (char) (member char '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)
                                             :test #'char=))
                      element)
               (parse-integer element))

              (t
               (funcall (compose #'quri:url-decode #'unescape) element))))
          (subseq (str:split #\/ relative-path-string) 1)))


(defun make-reference (reference-string)
  (let ((uri (quri:uri reference-string)))
    (make-instance 'reference
                   :relative-path (make-relative-path-list (quri:uri-fragment uri))
                   :uri (quri:render-uri (quri:copy-uri uri
                                                        :fragment nil
                                                        :query nil)))))


(defun relative-reference-p (reference)
  (string= "" (uri-of reference)))


(defun fetch-reference (uri)
  (-> uri
      (dex:get :read-timeout 10
               :connect-timeout 10
               :want-stream t)
      json:read-json))


(defun get-ref (spec)
  (json:getjso "$ref" spec))


(defun ref-p (spec)
  "A spec is a reference if it has only one key which is ``$ref``."

  (and (not (null spec))
       (nth-value 1 (json:getjso "$ref" spec))))


(defun get-subspec-by-ref (spec ref)
  (let ((path-list (if (stringp ref)
                       (make-relative-path-list ref)
                       ref)))

    (loop for component of-type (or integer string) in path-list

          if (stringp component)
            do (setf spec (json:getjso component spec))
          else
            do (setf spec (nth component spec))

          finally (return spec))))


(defun ensure-resolved (spec)
  "Resolve a reference if it exists, otherwise return the object.  Returns a second value indicating if there was a reference."

  (if (ref-p spec)
      (values (resolve spec) t)
      (values spec nil)))


(defun relative-to (reference)
  (let ((current-context (first (context-current-uri *context*))))
    (if (relative-reference-p reference)
        current-context
        (uri-of reference))))


(defun lookup (reference)
  (when-let ((schema (gethash (relative-to reference) (context-references *context*))))

    (get-subspec-by-ref schema (relative-path-of reference))))


(defun resolve (ref)
  (let ((reference (make-reference (get-ref ref))))
    (lookup reference)))
