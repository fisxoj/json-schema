(ql:quickload '(trivial-benchmark json-schema))

(defvar *schema* (json-schema.parse:parse #P"~/Downloads/schema"))

;; schema is the json-schema meta schema document from:
;; https://json-schema.org/specification-links.html#draft-2019-09-formerly-known-as-draft-8

(defvar *context*
  (json-schema:make-context
   *schema*
   :draft2019-09))

;;; Cached

(let ((data (json-schema.parse:parse "{\"type\": \"string\"}")))
  (trivial-benchmark:with-timing (1000)
    (json-schema:validate data
                          :context *context*)))

;; -                SAMPLES  TOTAL      MINIMUM  MAXIMUM   MEDIAN    AVERAGE    DEVIATION
;; REAL-TIME        1000     0.826      0        0.022     0.001     0.000826   0.000797
;; RUN-TIME         1000     0.826      0        0.022     0.001     0.000826   0.0008
;; USER-RUN-TIME    1000     0.781011   0        0.020644  0.000745  0.000781   0.000665
;; SYSTEM-RUN-TIME  1000     0.049933   0        0.000986  0         0.00005    0.000184
;; PAGE-FAULTS      1000     0          0        0         0         0          0.0
;; GC-RUN-TIME      1000     0.02       0        0.02      0         0.00002    0.000632
;; BYTES-CONSED     1000     213753664  195344   228976    228032    213753.66  16221.591
;; EVAL-CALLS       1000     0          0        0         0         0          0.0


;;; Uncached

(let ((data (json-schema.parse:parse "{\"type\": \"string\"}")))
  (trivial-benchmark:with-timing (1000)
    (json-schema:validate data
                          :schema *schema*
                          :schema-version :draft2019-09)))

;; -                SAMPLES  TOTAL      MINIMUM   MAXIMUM   MEDIAN    AVERAGE   DEVIATION
;; REAL-TIME        1000     203.185    0.148     1.471     0.185     0.203185  0.112807
;; RUN-TIME         1000     9.25       0.006     0.04      0.009     0.00925   0.002294
;; USER-RUN-TIME    1000     8.145081   0.003368  0.039067  0.008105  0.008145  0.002317
;; SYSTEM-RUN-TIME  1000     1.107377   0         0.004927  0.000994  0.001107  0.000967
;; PAGE-FAULTS      1000     0          0         0         0         0         0.0
;; GC-RUN-TIME      1000     0.08       0         0.03      0         0.00008   0.001464
;; BYTES-CONSED     1000     719780512  707728    751424    718160    719780.5  11026.181
;; EVAL-CALLS       1000     0          0         0         0         0         0.0
