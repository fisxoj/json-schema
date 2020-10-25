(defpackage :json-schema.formats
  (:use :cl :alexandria)
  (:export #:draft2019-09
           #:draft7
           #:draft6
           #:draft4
           #:draft3))

(in-package :json-schema.formats)

(define-constant +hostname-regex+ (ppcre:create-optimized-test-function "^[A-Za-z0-9][A-Za-z0-9\.\-]{1,255}$")
  :test 'string=)


(define-constant +unreserved-uri-characters+ (coerce  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-._~:@/?!$&'()*+,;=" 'list)
  :test 'equalp)


(defun datep (value)
  (handler-case (local-time:parse-rfc3339-timestring value
                                                     :allow-missing-time-part t)
    (local-time:invalid-timestring (e)
      (declare (ignore e))
      nil)))


(defun date-time-p (value)
  (handler-case (local-time:parse-rfc3339-timestring value
                                                     :allow-missing-time-part nil)
    (local-time:invalid-timestring (e)
      (declare (ignore e))
      nil)))


(defun emailp (value)
  (and (stringp value)
       (find #\@ value :test 'char=)))


(defun hostnamep (value)
  (and (stringp value)
       (ppcre:scan +hostname-regex+ value)
       (every (lambda (component) (< (length component) 64))
              (str:split #\. value))
       (not (str:ends-with-p "-" value))))


(defun ip-v4-address-p (value)
  (and (stringp value)
       (quri:ipv4-addr-p value)))


(defun ip-v6-address-p (value)
  (and (stringp value)
       (quri:ipv6-addr-p value)
       ;; https://github.com/fukamachi/quri/pull/34/files
       (<= (length (str:split #\: value)) 8)))


(defun json-pointer-p (value)
  (and (stringp value)
       (not (ppcre:scan "~([^01]|$)" value))
       (or (emptyp value)
           (char= (char value 0) #\/))))


(defun timep (value)
  (and (stringp value)
       ;; https://github.com/dlowe-net/local-time/issues/90
       (handler-case (local-time:parse-timestring value
                                                  :allow-missing-date-part t
                                                  :allow-missing-time-part nil
                                                  :fract-time-separators '(#\.))
         (local-time:invalid-timestring (e)
           (declare (ignore e))
           nil))))


(defun draft3-timep (value)
  (flet ((timelikep (value)
           (multiple-value-bind (matchp matches)
               (ppcre:scan-to-strings "([0-2]?[0-9]):([0-5][0-9]):([0-5][0-9])"
                                      value)
             (when (and matchp (= (length matches) 3))
               (and (<= 0 (parse-integer (svref matches 0)) 24)
                    (<= 0 (parse-integer (svref matches 1)) 59)
                    (<= 0 (parse-integer (svref matches 2)) 59))))))

    (and (stringp value)
         (timelikep value))))


(defun regexp (value)
  (handler-case (ppcre:parse-string value)
    (ppcre:ppcre-syntax-error (e)
      (declare (ignore e))
      nil)))


(defun uri (value)
  (handler-case
      (let ((uri (quri:uri value)))
        (not (emptyp (quri:uri-scheme uri))))
    (quri:uri-error ()
      nil)))


(defun uri-reference (value)
  (handler-case
      (let ((uri (quri:uri value)))
        (and
         (or (zerop (length (quri:uri-path uri)))
             (member (char (quri:uri-path uri) 0) +unreserved-uri-characters+ :test 'char=))
         (every (rcurry 'member +unreserved-uri-characters+ :test 'char=)
                (quri:uri-fragment uri))))
    (quri:uri-error ()
      nil)))


;;; draft checkers

(defmacro def-checker (name &rest types-plist)
  `(defun ,name (value type)
     (alexandria:eswitch (type :test #'string-equal)
       ,@(loop for (type function) on types-plist by #'cddr
               collecting `(,type (,function value))))))


(def-checker draft2019-09
  "date" datep
  "date-time" date-time-p
  "email" emailp
  "hostname" hostnamep
  "idn-email" emailp
  "ipv4" ip-v4-address-p
  "ipv6" ip-v6-address-p
  "json-pointer" json-pointer-p
  "regex" regexp
  "uri" uri
  "uri-reference" uri-reference)


(def-checker draft7
  "date" datep
  "date-time" date-time-p
  "email" emailp
  "hostname" hostnamep
  "idn-email" emailp
  "ipv4" ip-v4-address-p
  "ipv6" ip-v6-address-p
  "json-pointer" json-pointer-p
  "regex" regexp
  "time" timep
  "uri-reference" uri-reference
  "uri" uri)


(def-checker draft6
  "date-time" date-time-p
  "email" emailp
  "hostname" hostnamep
  "idn-email" emailp
  "ipv4" ip-v4-address-p
  "ipv6" ip-v6-address-p
  "json-pointer" json-pointer-p
  "regex" regexp
  "uri-reference" uri-reference
  "uri" uri)


(def-checker draft4
  "date-time" date-time-p
  "email" emailp
  "hostname" hostnamep
  "idn-email" emailp
  "ipv4" ip-v4-address-p
  "ipv6" ip-v6-address-p
  "json-pointer" json-pointer-p
  "regex" regexp
  "uri" uri)


(def-checker draft3
  "date" datep
  "date-time" date-time-p
  "email" emailp
  "host-name" hostnamep
  "idn-email" emailp
  "ipv4" ip-v4-address-p
  "ipv6" ip-v6-address-p
  "json-pointer" json-pointer-p
  "regex" regexp
  "time" draft3-timep
  "uri" uri)
