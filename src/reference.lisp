(defpackage json-schema.reference
  (:use :cl :alexandria :cl-arrows)
  (:local-nicknames (:utils :json-schema.utils)
                    (:parse :json-schema.parse))
  (:export #:make-reference
           #:with-context
           #:push-context
           #:relative-reference-p
           #:escape
           #:unescape
           #:with-pushed-context
           #:get-subspec-by-ref
           #:ensure-resolved
           #:resolve
           #:get-ref))

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
  "A container for all state related to resolving references, namely: a stack of context urls"
  (uri-stack nil :type (trivial-types:proper-list string))
  (references (make-hash-table :test 'equal) :type hash-table)
  (named-references (make-hash-table :test'equal) :type hash-table))


(defun default-id-fun (schema)
  (if (typep schema 'utils:object)
      (utils:object-get "$id" schema "")
      (values "" nil)))


(defmacro with-context (() &body body)
  `(let ((*context* (make-context)))
     ,@body))


(defun push-context (schema &optional (id-fun #'default-id-fun))
  ;; (format t "~& >pc: pushing schema id ~S.~%"
  ;;         (funcall id-fun schema))

  (let ((uri (make-uri-without-fragment (funcall id-fun schema))))
    (push uri (context-uri-stack *context*))
    (setf (gethash uri (context-references *context*)) schema)))


(defun pop-context ()
  (pop (context-uri-stack *context*)))


(defmacro with-pushed-context ((schema &optional (id-fun ''default-id-fun)) &body body)
  (once-only (schema)
    `(unwind-protect
          (progn
            (push-context ,schema ,id-fun)
            ,@body)
       (pop-context))))


(defun get-current-uri ()
  (assert (not (null (context-uri-stack *context*))) nil
          "No uris in context stack.")

  (first (context-uri-stack *context*)))


(defun get-current-schema ()
  (gethash (get-current-uri) (context-references *context*)))


(defun unescape (string)
  "Unescape a string to replace ~0 and ~1 with ~ and /."

  (with-output-to-string (out)
    (with-input-from-string (in string)

      (loop for char = (read-char in nil nil)
            for next-char = (peek-char nil in nil nil)

            while char

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
  ((relative-path :type (or string ;; location-independent reference
                            (trivial-types:proper-list string)) ;; json pointer
                  :initarg :relative-path
                  :accessor relative-path-of)
   (uri :type string
        :initarg :uri
        :accessor uri-of)))


(defmethod print-object ((object reference) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "uri: ~S path: ~S"
            (uri-of object)
            (relative-path-of object))))


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


(defun make-uri-without-fragment (uri)
  (-> (typecase uri
        (quri:uri uri)
        (string (quri:uri uri)))
      (quri:copy-uri :fragment nil
                     :query nil)
      quri:render-uri))


(defun make-reference (reference-string)
  (let* ((uri (quri:uri reference-string))
         (fragment (quri:uri-fragment uri)))

    (make-instance 'reference
                   :relative-path (unless (zerop (length fragment))
                                    (if (char= (char fragment 0) #\/)
                                        ;; json-pointer
                                        (make-relative-path-list fragment)
                                        ;; location-independent
                                        (str:concat "#" fragment)))
                   :uri (make-uri-without-fragment uri))))


(defun relative-reference-p (reference)
  (check-type reference reference)
  (string= "" (uri-of reference)))


(defun fetch-reference (uri)
  (flet ((store-schema (schema)
           (setf (gethash uri (context-references *context*)) schema)))
    (-> uri
        (dex:get :read-timeout 10
                 :connect-timeout 10)
        babel:octets-to-string
        parse:parse
        store-schema)))


(defun get-ref (spec)
  (utils:object-get "$ref" spec))


(defun ref-p (spec)
  "A spec is a reference if it has only one key which is ``$ref``."

  (and (not (null spec))
       (nth-value 1 (get-ref spec))))


(defun get-subspec-by-reference (reference)
  (let ((relative-path (relative-path-of reference)))

    (cond
      ((stringp relative-path)
       ;; location-independent

       ;; this unless bears some explaining: If we've encountered a location-independent identifier, it means that we could be looking for anything anywhere in this schema :'(.  Anecdotally, this feature doesn't seem widely-used, so we don't walk the document tree until we find out it is, the first time someone tries to use a named reference.  So, when the named references hash table is empty for a uri key, we generate it by walking that whole schema document and finding all the named anchors.
       (unless (gethash (uri-of reference) (context-named-references *context*))
         (setf (gethash (uri-of reference) (context-named-references *context*))
               (alist-hash-table (collect-subschemas (get-current-schema))
                                 :test 'equal)))

       (gethash relative-path (gethash (uri-of reference) (context-named-references *context*))))

      ((proper-list-p relative-path)
       ;; json pointer
       (loop with spec = (get-current-schema)
             for component of-type (or integer string) in relative-path

             if (stringp component)
               do (setf spec (utils:object-get component spec))
             else ;; an integer
               do (setf spec (nth component spec))

             finally (return spec))))))


(defun ensure-resolved (spec)
  "Resolve a reference if it exists, otherwise return the object.  Returns a second value indicating if there was a reference."

  (if (ref-p spec)
      (values (resolve spec) t)
      (values spec nil)))


(defun relative-to (reference)
  (quri:render-uri (quri:merge-uris (uri-of reference) (get-current-uri))))


(defun lookup (reference)
  "Look up a schema by reference in the ``*context*``.  Returns ``(values schema new-context-p)``.  ``new-context-p`` indicates that this schema is a new document that should be pushed to the context stack when visited."

  (let ((schema-uri (relative-to reference)))
    (unless (gethash schema-uri (context-references *context*))
              (fetch-reference (relative-to reference)))

    (values (get-subspec-by-reference reference) (not (emptyp (uri-of reference))))))


(defun resolve (ref)
  "Resolves a reference schema object to the referred-to schema."

  (lookup (make-reference (get-ref ref))))


(defun collect-subschemas (schema &key (id-fun #'default-id-fun))
  "Collect all named subschemas into an alist of (name . schema-object)."

  (when (typep schema 'utils:object)
    (let ((subschema-ids (loop for key in (utils:object-keys schema)
                               for value = (utils:object-get key schema)

                               appending (collect-subschemas value))))
      (multiple-value-bind (id found-p) (funcall id-fun schema)
        (if found-p
            (list* (cons id schema) subschema-ids)
            subschema-ids)))))
