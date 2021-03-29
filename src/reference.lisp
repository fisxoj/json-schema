(defpackage json-schema.reference
  (:use :cl :alexandria :arrows)
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
           #:resolve
           #:get-ref
           #:with-resolved-ref
           #:*resolve-remote-references*

           ;; conditions
           #:remote-reference-error
           #:fetching-not-allowed-error
           #:reference-error
           #:nested-reference-error
           #:with-pushed-id
           #:get-id-fun-for-draft))

(in-package :json-schema.reference)

;;; NOTE: inside this file, `ref` refers to the property of a json object, and `reference` is the structure object we use internally for bookkeeping.


(defvar *context* nil
  "The lookup context for references.")


(defparameter +max-lookup-depth+ 100
  "Maximum number of nested references to allow before throwing a :class:`nested-reference-error`.")


(defvar *current-lookup-depth* 0
  "Dynamic variable for tracking reference nesting depth.")


(defvar *resolve-remote-references* t
  "Whether to download other schemas for references.  Will error if another uri is referenced in a schema and this var is set to ``nil``.")


(defvar *id-fun* 'default-id-fun
  "A default function for getting ids from schemas.  Should return (values id found-p) like gethash.")


(defmacro with-lookup-depth-tracking (&body body)
  `(unwind-protect
        (progn
          (incf *current-lookup-depth*)
          (when (> *current-lookup-depth* +max-lookup-depth+)
            (error 'nested-reference-error))
          ,@body)
     (decf *current-lookup-depth*)))


(define-condition reference-error (error)
  ((message :type string
            :initarg :message))

  (:report (lambda (c stream)
             (format stream "JSON Schema reference error: ~a"
                     (slot-value c 'message)))))


(define-condition fetching-not-allowed-error (reference-error)
  ((remote-uri :type string
               :initarg :uri))

  (:report (lambda (c stream)
             (format stream "JSON Schema reference error: need to fetch ~S but *RESOLVE-REMOTE-REFERENCES* is nil."
                     (slot-value c 'remote-uri)))))


(define-condition remote-reference-error (reference-error)
  ((remote-uri :type string
               :initarg :uri))
  (:report (lambda (c stream)
             (format stream "JSON Schema reference error, when fetching ~S: ~a"
                     (slot-value c 'remote-uri)
                     (slot-value c 'message)))))


(define-condition nested-reference-error (reference-error)
  ()
  (:report (lambda (c stream)
             (format stream "Reference nesting depth of ~d exceeded."
                     +max-lookup-depth+))))


(defstruct context
  "A container for all state related to resolving references, namely: a stack of context urls"
  (uri-stack nil :type (trivial-types:proper-list string))
  (references (make-hash-table :test 'equal) :type hash-table)
  (named-references (make-hash-table :test'equal) :type hash-table))


(defun default-id-fun (schema)
  (if (typep schema 'utils:object)
      (utils:object-get "$id" schema "")
      (values "" nil)))


(defun draft2019-09-id-fun (schema)
  "An id extraction function that also pays attention to $anchor properties which provide only location-independent references."

  (if (typep schema 'utils:object)
      (multiple-value-bind (id id-found-p) (utils:object-get "$id" schema)
        (multiple-value-bind (anchor anchor-found-p) (utils:object-get "$anchor" schema)
          (values (quri:merge-uris (or (str:concat "#" anchor) "")
                                   (or id ""))
                  (or id-found-p anchor-found-p))))
      (values "" nil)))


(defun draft4-id-fun (schema)
  (if (typep schema 'utils:object)
      (utils:object-get "id" schema "")
      (values "" nil)))


(defun get-id-fun-for-draft (schema-version)
  "Selects an id function that's appropriate for each schema draft."

  (ecase schema-version
    (:draft2019-09
     'draft2019-09-id-fun)
    ((or :draft7 :draft6)
     'default-id-fun)
    (:draft4
     'draft4-id-fun)))


(defmacro with-context ((&optional (id-fun ''default-id-fun)) &body body)
  `(let ((*context* (make-context))
         (*id-fun* ,id-fun))
     ,@body))


(defun push-context (schema &optional (id-fun *id-fun*))
  ;; (format t "~&> pc: pushing schema id ~S.~%"
  ;;         (funcall id-fun schema))

  (let ((uri (make-uri-without-fragment (funcall id-fun schema))))
    (push uri (context-uri-stack *context*))

    (unless (gethash uri (context-references *context*))
      (setf (gethash uri (context-references *context*)) schema))

    (populate-named-references-for-schema schema
                                          :id-fun id-fun
                                          :uri (quri:uri uri))))


(defun pop-context ()
  (pop (context-uri-stack *context*)))


(defmacro with-pushed-id ((id) &body body)
  (once-only (id)
    `(unwind-protect
          (progn
            (push (quri:render-uri (quri:merge-uris (make-uri-without-fragment ,id) (get-current-uri))) (context-uri-stack *context*))
            ,@body)
       (pop-context))))


(defmacro with-pushed-context ((schema &optional (id-fun '*id-fun*)) &body body)
  (once-only (schema)
    `(unwind-protect
          (progn
            (push-context ,schema ,id-fun)
            ,@body)
       (pop-context))))


(defmacro with-resolved-ref ((ref resolved-schema &optional (id-fun '*id-fun*)) &body body)

  (once-only (ref)
    (with-gensyms (resolved-p schema-uri id found-p)
      `(multiple-value-bind (,resolved-schema ,resolved-p ,schema-uri)
           (resolve ,ref)

         (if ,resolved-p
             (unwind-protect
                  (progn
                    ;; FIXME: it's definitely happening here
                    (push-context (gethash ,resolved-schema (context-references *context*))
                                  (lambda (s)
                                    ;; If the schema contains an id, great.  If not, use the url we fetched it from.
                                    (multiple-value-bind (,id ,found-p)
                                        (funcall ,id-fun s)
                                      (if ,found-p
                                          ,id
                                          ,schema-uri))))

                    ,@body)
               (pop-context))
             (progn ,@body))))))


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
  (let* ((uri (quri:merge-uris reference-string (get-current-uri)))
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
  (unless *resolve-remote-references*
    (error 'fetching-not-allowed-error
           :uri uri))

  (handler-case
      (flet ((store-schema (schema)
               (setf (gethash uri (context-references *context*)) schema)
               (populate-named-references-for-schema schema
                                                     :id-fun #'default-id-fun
                                                     :uri (quri:uri uri))
               schema))

        (-> uri
            (dex:get :read-timeout 10
                     :connect-timeout 10
                     :force-binary t)
            babel:octets-to-string
            parse:parse
            store-schema))
    (usocket:connection-refused-error (error)
      (declare (ignore error))
      (error 'remote-reference-error
             :message "connection refused"
             :uri uri))
    (usocket:timeout-error (error)
      (declare (ignore error))
      (error 'remote-reference-error
             :message "connection timed out"
             :uri uri))
    (dex:http-request-not-found (error)
      (declare (ignore error))
      (error 'remote-reference-error
             :message "document not found"
             :uri uri))))


(defun get-ref (spec)
  (utils:object-get "$ref" spec))


(defun ref-p (spec)
  "A spec is a reference if it has only one key which is ``$ref``."

  (and (not (null spec))
       (nth-value 1 (get-ref spec))))


(defun absolute-uri (reference)
  "Return an absolute URI for the reference in the current context."

  (quri:render-uri (quri:merge-uris (uri-of reference) (get-current-uri))))


(defun lookup (reference)
  "Look up a schema by reference in the ``*context*``.  Returns ``(values schema new-context-p)``.  ``new-context-p`` indicates that this schema is a new document that should be pushed to the context stack when visited."

  (with-lookup-depth-tracking
    (let* ((schema-uri (absolute-uri reference))
           (relative-path (relative-path-of reference)))

      (flet ((ensure-schema-fetched ()
               (unless (gethash schema-uri (context-references *context*))
                 (fetch-reference schema-uri)))
             (new-context-p ()
               ;; some loookups might change the uri by id,
               ;; so this is a function to calculate it at return time
               (not (string= schema-uri (get-current-uri)))))

        (etypecase relative-path
          (null
           ;; no relative part
           (ensure-schema-fetched)

           (values (gethash schema-uri (context-references *context*))
                   (new-context-p)
                   schema-uri))

          (string
           ;; location-independent
           (ensure-schema-fetched)

           (values (gethash relative-path (gethash (uri-of reference) (context-named-references *context*)))
                   (new-context-p)
                   schema-uri))

          (proper-list
           ;; json pointer

           (ensure-schema-fetched)

           (multiple-value-bind (schema found-p)
               (gethash schema-uri (context-references *context*))

             (if found-p
                 (values (loop with spec = schema
                               for (component . rest) on relative-path by #'cdr

                               if (stringp component)
                                 do (progn
                                      (setf spec (utils:object-get component spec))
                                      (when-let ((id (funcall *id-fun* spec)))
                                        ;; if we're in the same object as an id, leave that off
                                        (when rest
                                          (setf schema-uri (quri:render-uri (quri:merge-uris id schema-uri))))))
                               else ;; an integer
                               do (setf spec (nth component spec))

                               finally (return spec))
                         (new-context-p)
                         schema-uri)
                 (values nil nil schema-uri)))))))))


(defun resolve (ref)
  "Resolves a reference schema object to the referred-to schema."

  (lookup (make-reference (get-ref ref))))



(defun collect-subschemas (schema &key (id-fun *id-fun*) current-uri properties-p)
  "Collect all named subschemas into an alist of (name . schema-object)."

  (when (typep schema 'utils:object)
    (multiple-value-bind (id found-p) (funcall id-fun schema)
      (let* ((current-uri (cond
                            ;; properties-p: we don't want to collect any "$id" properties
                            ;; that are direct children of a "properties" field.  This is a
                            ;; bit hacky, but is important because this happens in the
                            ;; meta-schemas.
                            (properties-p
                             current-uri)
                            ((and found-p current-uri)
                             (quri:merge-uris id current-uri))
                            (found-p
                             (quri:uri id))
                            (current-uri
                             current-uri)))
             (subschema-ids (loop for key in (utils:object-keys schema)
                                  for maybe-schema = (utils:object-get key schema)

                                  appending (collect-subschemas maybe-schema
                                                                :id-fun id-fun
                                                                :current-uri current-uri
                                                                :properties-p (utils:object-get "properties" schema)))))
        (if (and found-p (not properties-p))
            (list* (cons current-uri schema) subschema-ids)
            subschema-ids)))))


(defun populate-named-references-for-schema (schema &key (id-fun *id-fun*) uri)
  "Takes an alist of (uri . schema) and populates the appropriate hash tables in the named references slot of the context.  Takes the output from collect subschemas, which may return named references for many uri's, since documents are allowed to insist they have whatever uri they want whenever they want."

  (loop for (uri . schema) in (collect-subschemas schema :id-fun id-fun :current-uri uri)
        for reference = (make-reference uri)

        ;; do (format t "~&Setting named reference ~S for document ~S."
        ;;            (relative-path-of reference)
        ;;            (uri-of reference))

        unless (gethash (uri-of reference) (context-named-references *context*))
          do (setf (gethash (uri-of reference) (context-named-references *context*))
                   (make-hash-table :test 'equal))

        when (relative-path-of reference)
          do (setf (gethash (relative-path-of reference) (gethash (uri-of reference) (context-named-references *context*)))
                   schema)

        unless (gethash (uri-of reference) (context-references *context*))
          ;; pretend this is a document we downloaded...
          do (if (relative-path-of reference)
                 ;; We're expected to resolve against these uris and not fetch them, soooo....
                 (setf (gethash (uri-of reference) (context-references *context*))
                       :stub)
                 (setf (gethash (uri-of reference) (context-references *context*))
                       schema))))
