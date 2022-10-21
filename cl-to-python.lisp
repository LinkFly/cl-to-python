;(ql:quickload :flexi-streams)
;(ql:quickload :bordeaux-threads)

(in-package :cl-to-python)

;(defparameter *python-cmd* "python -u")
(defparameter *python-cmd* "python")
(defparameter *python* nil)
(defparameter *start-isnt-alive-p* t)
;(defparameter *force-restart-always* t)
(defparameter *force-restart-always* nil)

(defparameter *custom-random-state* (make-random-state t))
(defparameter *error-prefix* "_<error_")

;; TODO Binding with python (using this constant also python)
(defparameter *unknown-uuid* "<unknown_uuid_----------------------->")
(defparameter *results-for-unknown-commands* nil)
(defparameter *None* 'None)
(defparameter *wait-result-period* 0.1)
;(defparameter *wait-result-period* 1)
(defparameter *wait-output-period* 0.1)
;(defparameter *wait-output-period* 1)
(defparameter *last-error* nil)
;(defparameter *debug-python-worker* t)
(defparameter *debug-python-worker* nil)

(defstruct python-conn proc-info worker results outputs (worker-exit-p nil))

(defun results-hash ()
  (python-conn-results *python*))

(defun outputs-hash ()
  (python-conn-outputs *python*))

(defun worker-exit-p ()
  (python-conn-worker-exit-p *python*))

(defun unknown-uuid-p (uuid) (string= uuid *unknown-uuid*))

(defun gen-uid ()
  (let ((*random-state* *custom-random-state*))
    (format nil "[~a]" (princ-to-string (uuid:make-v4-uuid)))))

(defparameter *uuid-len* (length (gen-uid)))

;;;;;;;;;;; Python output ;;;;;;;;;;;;
(defstruct python-output str-output)

(defmethod print-object ((obj python-output) out)
  (print-unreadable-object (obj out :type t)
    (format out "~s" (python-output-str-output obj))))
;(make-python-output :str-output "Some data")
#|(make-python-output :str-output "Some oute
Newline blabla
Newline 2")
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-py-script ()
  (uiop:native-namestring
   (truename (asdf:component-pathname
              (asdf:find-component
               :cl-to-python "python-code")))))
;(get-py-script)

(defun prepare-cmd (op cmd &aux #|prepared-cmd|#)
  ;(setf prepared-cmd (concatenate 'string op (princ-to-string (length cmd)) (string #\Newline) cmd))
  #|(if (and (string= "e"op)
           (string= "" cmd))
      (safing-replace-special prepared-cmd))|#
  (concatenate 'string op (princ-to-string (length cmd)) (string #\Newline) cmd)
  )

(defun parse-cmd (in-cmd &aux op npos len cmd)
  (setf op (subseq in-cmd 0 1))
  (setf npos (position #\Newline in-cmd))
  (setf len (read-from-string (subseq in-cmd 1 npos)))
  (setf cmd (subseq in-cmd (1+ npos) (+ npos 1 len)))
  `(:op ,op :cmd ,cmd)
  )

(defun get-op (spec)
                                        ;(setf spec (parse-cmd in-cmd))
  (getf spec :op))

(defun get-cmd (spec)
  (getf spec :cmd))


(defun safing-replace-special (str)
  (if (search "ee00\\\\nn" str)
      (error "Found special string subsequence"))
  (ppcre:regex-replace-all "e0\\n" str "ee00\\\\nn"))
;(safing-replace-special (prepare-cmd "e" ""))
(assert (string= (safing-replace-special (uiop:strcat "___e0" (string #\Newline) "___"))
                 "___ee00\\nn___"))

;; For info
(defun disable-safing-replace (str)
  (ppcre:regex-replace-all "ee00\\\\nn" str (uiop:strcat "e0" (string #\Newline))))
(assert (string= (disable-safing-replace "___ee00\\nn___")
                 (uiop:strcat "___e0" (string #\Newline) "___")))

(defun process-op-cmd (op cmd &aux prepared-cmd)
  (setf prepared-cmd (prepare-cmd op cmd)))

(defun write-to-python (op cmd &aux prepared-cmd)
  ;(setf prepared-cmd (process-op-cmd op cmd))
  (setf prepared-cmd (prepare-cmd op cmd))
  (log-info prepared-cmd)
  (write-sequence (string-to-octets prepared-cmd)
                  (uiop:process-info-input (python-conn-proc-info *python*)))
  (finish-output (uiop:process-info-input (python-conn-proc-info *python*))))
;(write-to-python "e" "")

(defun maybe-restart-python ()
  (if *force-restart-always*
      (py-start)
      (if (and *start-isnt-alive-p* *python* (not (uiop/launch-program:process-alive-p (python-conn-proc-info *python*))))
          (py-start))))

(defun deserialize-python-value (seq bytes-count)
  #|python_to_lisp_type = {
  bool: "BOOLEAN",
  type(None): "NULL",
  int: "INTEGER",
  float: "FLOAT",
  complex: "COMPLEX",
  list: "VECTOR",
  dict: "HASH-TABLE",
  str: "STRING",
  }

  lisp_type_to_sym = {
  "BOOLEAN": 'b',
  "NULL": 'n',
  "INTEGER": 'i',
  "FLOAT": 'f',
  "COMPLEX": 'c',
  "VECTOR": 'v',
  "HASH-TABLE": 'h',
  "STRING": 's',
  }
  |#
  (let* ((sval (octets-to-string seq))         
         ;; TODO Using view array, using bytes (rid of translate to string)
         (uuid (subseq sval 0 *uuid-len*))
         (type (elt sval *uuid-len*))
         (val (subseq sval (1+ *uuid-len*) bytes-count)))
    (log-info "deserialize-python-value: uuid = ~s, type = ~s, val = ~s" uuid type val)
    (list (handler-case (case type
                          (#\s val)
                          (#\i (parse-integer val))
                          (#\n *None*)
                          (#\v (parse-array val))
                          (#\b (cond
                                 ((string= "True" val) t)
                                 ((string= "False" val) nil)
                                 (t (error "Bad string for boolean type"))))
                          (#\_ (progn
                                 (if (search *error-prefix* val)
                                     (error val)
                                     (progn
                                       (warn "Returned native data presentation: ~s" val)
                                       val))))
                          (#\p (values))
                          (#\e (error 'python-error :text (format nil "Error from python: ~s" val)))
                          (#\o (make-python-output :str-output val))
                          (t (error "Not impelemented for type: ~s" type)))
                                        ;(python-error ())
            (python-error (err) err))
          uuid)
    )
  )

(defparameter *slime-output* *standard-output*)
(defparameter *slime-error-output* *error-output*)

(defun log-info (msg &rest args)
  (format *slime-output* "~%~a" (apply #'format nil msg args))
  (finish-output *slime-output*))


(defun log-error (msg &rest args)
  (format *slime-error-output* "~%~%[ERROR]: ~a~%" (apply #'format nil msg args))
  (finish-output *error-output*))

(defun set-wait-res (key-uuid out-hash)
  (setf (gethash key-uuid out-hash) (list nil nil)))

(defun set-res (key-uuid res out-hash)
  (if (unknown-uuid-p key-uuid)
      (progn
        (warn "Added result for unknown uuid to *results-for-unknown-commands*, result = ~s" res)
        (push res *results-for-unknown-commands*))
      (multiple-value-bind (val present-p)
          (gethash key-uuid out-hash)
        (if (not present-p)
            (error "Not waiting result for uid = ~s" key-uuid))
        (destructuring-bind (saved  &rest rest)
            (coerce val 'list)
          (declare (ignore rest))
          (if saved (error "Result already saved"))
          (setf (gethash key-uuid out-hash) (vector t res))))))

(defun get-res (key-uuid out-hash)
  (log-info "results-hash = ~s" out-hash)
  (multiple-value-bind
        (val present-p)
      (gethash key-uuid out-hash)
    (if (not present-p)
        (values nil nil)
        (destructuring-bind (has-come-p res)
            (coerce val 'list)
          (values res has-come-p)))))

#|
(defparameter *test-py* nil)
(defun test-run ()
  (setf *test-py* (uiop:launch-program (uiop:strcat *python-cmd* " " (get-py-script)) :output :stream :input :stream :error-output :stream)))
(defun raise-err (&aux op-seq proc)
  (setf op-seq (make-sequence '(vector (unsigned-byte 8)) 1))
  (read-sequence op-seq (uiop:process-info-output proc) :end 1)
  (log-info (format nil "=== OK ===: ~s" (octets-to-string op-seq))))
|#
;(worker-python)

(define-condition python-error (error)
  ((text :initarg :text :reader text)))

(defun %worker-python (&aux slen len)
  (loop with python-output = (uiop:process-info-output (python-conn-proc-info *python*))
        with payload-seq
        with payload-count = 0
        ;; For describe of stages
        with read-op = 'read-op
        with read-len = 'read-len
        with read-payload = 'read-payload
        with stage = read-op
        :while (not (worker-exit-p))
        do
        (log-info "=== Current stage = ~s" stage)
        (case stage
          (read-op (let (op-code)
                     (log-info "=== before read op")
                     (handler-case (setf op-code (read-byte python-output))
                       (error (e)
                         (format t "Error on ~s: ~s" stage e)
                         (loop-finish)))
                     (log-info "=== after read op, op-code = ~s, op = ~a" op-code (code-char op-code))
                     
                     (if (not op-code)
                         (progn
                           ;; TODO 0.1 to variable
                           (sleep 0.1))
                         (let ((op (code-char op-code)))
                           (log-info "=== Analysing op: ~s" op op-code)
                           (if (not (or (char= #\e op)
                                        (char= #\r op)))
                               (error "OP `~s` dont supported (code: ~s)" op op-code))
                           ;; TODO 0.1 to variable
                           (setf stage read-len)))))
          (read-len (progn
                      (log-info "=== before read slen")
                      (handler-case (setf slen (read-line python-output))
                        (error (e)
                          (format t "Error on ~s: ~s" stage e)
                          (loop-finish)))
                      (log-info "=== after read slen, slen = ~s" slen)
                      (setf len (ignore-errors (parse-integer slen)))
                      (when (not len)
                        (progn (error "Bad parsing len: ~s" slen)
                               (loop-finish)))
                      (log-info "=== before set stage: " read-payload)
                      (setf stage read-payload)
                      (log-info "=== after set stage, stage = ~s" stage)))
          (read-payload (let (bytes-count)
                          (log-info "=== before read payload")
                          (if (zerop payload-count)
                              ;; Start read payload
                              (setf payload-seq (make-sequence '(vector (unsigned-byte 8)) len)))
                                        ;(setf payload-view (make-array (- len payload-count) :element-type '(unsigned-byte 8) :displaced-to payload-seq :displaced-index-offset payload-count))
                          (handler-case (setf bytes-count (read-sequence payload-seq python-output :start payload-count :end len))
                            (error (e)
                              (format t "Error on ~s: ~s" stage e)
                              (loop-finish)))
                          (log-info "=== after read payload, bytes-count = ~s, payload-seq as string = ~s, payload-view as string = ~s"
                                    bytes-count (octets-to-string payload-seq) (octets-to-string (subseq payload-seq payload-count len)))
                          (if (> (+ payload-count bytes-count) len)
                              (error "Bad algorithm for reading payload"))
                          (let ((readed-bytes (+ payload-count bytes-count)))
                            (if (= readed-bytes len)
                                (progn
                                  (destructuring-bind (val uuid)
                                      (deserialize-python-value payload-seq readed-bytes)
                                    (set-res uuid val (if (typep val 'python-output)
                                                          (outputs-hash)
                                                          (results-hash)))
                                    (log-info (format nil "After set-res: res = ~s, uuid = ~s" val uuid)))
                                  (setf payload-count 0
                                        payload-seq nil
                                        stage read-op)))))))))

(defun worker-python ()
  (if (not (python-alive-p))
      (progn
        (error "Python don't running (maybe required to call (py-start)?)"))
      (handler-case (%worker-python)
        (error (err)
          (log-error "in worker-python: ~a " err)
          (setf *last-error* err)
          (if *debug-python-worker* (error err))))))
  

(defun run-python-worker (&optional (python *python*))
  (log-info "Before run worker")
  (setf (python-conn-worker python)
        (make-thread 'worker-python :name "Python thread" :initial-bindings (cons (list '*python* 'identity *python*) *default-special-bindings*))
        )
  (log-info "After run worker: ~s" (python-conn-worker python)))
  
(defun py-start (&aux python-proc-info)
  (if (python-alive-p) (py-stop))
  (setf python-proc-info (uiop:launch-program (uiop:strcat *python-cmd* " " (get-py-script)) :output :stream :input :stream :error-output :stream))
  (setf *python* (make-python-conn :proc-info python-proc-info
                                   ;; TODO Move to constructor
                                   :results (make-hash-table :test 'equal)
                                   :outputs (make-hash-table :test 'equal)))
  (run-python-worker *python*))

(defun python-alive-p ()
  #|(break "in python-alive-p, wait result = ~s" (if (and *python* (python-conn-proc-info *python*))
                                                   (uiop:process-alive-p (python-conn-proc-info *python*))))|#
  (if (and *python* (python-conn-proc-info *python*))
      (uiop:process-alive-p (python-conn-proc-info *python*))))

(defun py-kill (&aux process)
  (setf process (slot-value (python-conn-proc-info *python*) 'uiop/launch-program::process))
  (when (sb-ext:process-alive-p process)
    ;; Note: uiop:terminate-process - don't working this
    (sb-ext:process-kill process 15)))

(defun py-stop (&optional (kill-python-cmd-p t))
  (when (python-alive-p)
    (write-to-python "f" "")
    ;; TODO 0.1 to variable
    (sleep 0.1)
    (let ((thread-python (python-conn-worker *python*)))
      (if (and thread-python (thread-alive-p thread-python))
          (destroy-thread thread-python))
      (if (and kill-python-cmd-p)
          ;; TODO kill python child
          (py-kill)))
    
    nil))

;; TODO move to top
;(py-start)
(defparameter *continue-sent-empty-command* t)
;(defparameter *continue-sent-empty-command* nil)

(defun wait-value-loop (uuid out-hash)
  (loop 
    with res
    while t
    finally (return res)
    do
    (multiple-value-bind (val has-come-p)
        (progn
          (log-info "trying getting res, is output wait: ~a" (eql out-hash (outputs-hash)))
          (let ((val-and-has-come-p (multiple-value-list (get-res uuid out-hash))))
            (log-info "got results: ~s" val-and-has-come-p)
            (values-list val-and-has-come-p)))
      (if has-come-p
          (progn
            (setf res val)
            (remhash uuid out-hash)
            (loop-finish))
          ;; Note: It required than wakeup python (it hanging on read input on my system (windows))
          (progn
            (log-info "res don't got")
            (if *continue-sent-empty-command*
                ;; TODO Rid of using uuid for stub message
                (let ((uuid-stub (gen-uid)))
                  (write-to-python "e" uuid-stub)))
            ;; TODO 1 to variable
            (sleep *wait-result-period*))
          ))))

(defun send-to-python (cmd &optional (op "e") &aux #|seq bytes-count slen th|# uuid)
  (maybe-restart-python)
  (if (not (python-alive-p))
      (error "Python isn't running"))
  (setf uuid (gen-uid))
  (setf cmd (uiop:strcat uuid cmd))
  (set-wait-res uuid (results-hash))
  (set-wait-res uuid (outputs-hash))
  (write-to-python op cmd)
  (values (wait-value-loop uuid (results-hash))
          (wait-value-loop uuid (outputs-hash))))

(defun py-eval (cmd)
  (send-to-python cmd "e"))

(defun py-peval (cmd)
  (send-to-python cmd "p"))

(defun py-exec (cmd)
  (send-to-python cmd "x"))

;;;;;;;;;;;;;;



