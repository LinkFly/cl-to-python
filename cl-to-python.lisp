;(ql:quickload :flexi-streams)
;(ql:quickload :bordeaux-threads)

(defpackage :cl-to-python
  (:use :cl :uiop :flexi-streams :bordeaux-threads)
  (:export #:python-run #:python-stop #:py-eval #:py-exec))

(in-package :cl-to-python)

(defparameter *python-cmd* "python")
(defparameter *python* nil)
(defparameter *start-isnt-alive-p* t)
;(defparameter *force-restart-always* t)
(defparameter *force-restart-always* nil)

(defun get-py-script ()
  (uiop:native-namestring
   (truename (asdf:component-pathname
              (asdf:find-component
               :cl-to-python "python-code")))))
;(get-py-script)

(defun python-alive-p ()
  (if *python*
      (uiop:process-alive-p *python*)))

(defun python-stop (&optional (kill-python-cmd-p nil))
  (when (python-alive-p)
    (write-to-python "f" "")
    (if (and kill-python-cmd-p
             (sb-ext:process-kill (slot-value *python* 'UIOP/LAUNCH-PROGRAM::PROCESS) 15)) 
        (progn
          (setf *python* nil)))
    nil))

(defun python-run ()
  (if (python-alive-p) (python-stop))
  (setf *python* (uiop:launch-program (uiop:strcat *python-cmd* " " (get-py-script)) :output :stream :input :stream :error-output :stream)))

;(python-run)

(defun prepare-cmd (op cmd)
  (concatenate 'string op (princ-to-string (length cmd)) (string #\Newline) cmd))

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

(defun write-to-python (op cmd &aux prepared-cmd)
  (setf prepared-cmd (prepare-cmd op cmd))
  (log-info prepared-cmd)
  (write-sequence (string-to-octets prepared-cmd)
                  (uiop:process-info-input *python*))
  (finish-output (uiop:process-info-input *python*)))
;(write-to-python "e" "tmp")



(defun maybe-restart-python ()
  (if *force-restart-always*
      (python-run)
      (if (and *start-isnt-alive-p* (not (uiop/launch-program:process-alive-p *python*)))
          (python-run))))

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
  (let ((type (elt seq 0))
        (val (subseq seq 1 bytes-count)))
    (case type
      (#.(char-code #\s) (octets-to-string val))
      (#.(char-code #\i) (values (parse-integer (octets-to-string val))))
      (#.(char-code #\n) (values))
      (t (error "Not impelemented for type: ~s" (code-char type))))
    )
  )

(defparameter *slime-output* *standard-output*)

(defun log-info (msg)
  (format *slime-output* "~%~s" msg)
  (finish-output *slime-output*))

(defun create-thread-func (op cmd)
  (lambda ()
    (log-info `(write-to-python ,op ,cmd))
    (sleep 1)
    (write-to-python op cmd)
    #|(loop while t
          do
          (write-to-python "e" "1")
          (sleep 1))|#
                                        ;(write-to-python "e" "cmd")
                                        ;(loop repeat 10 do (write-to-python cmd))
    ))
(defun send-to-python (cmd &optional (op "e") &aux seq bytes-count slen #|th|#)
  (maybe-restart-python)
  
  ;(return-from read-from-python (write-to-python cmd))
  ;(setf th (make-thread (create-thread-func op cmd)))
  ;(log-info "before-read-1")
  (setf seq (make-sequence '(vector (unsigned-byte 8)) 10000))
  (write-to-python op cmd)
  (if (/= 1 (read-sequence seq (uiop:process-info-output *python*) :end 1))
      (error "dont read OP"))
  ;(log-info "after-read-1")
  (if (not (or (= #.(char-code #\e) (elt seq 0))
               (= #.(char-code #\r) (elt seq 0))))
      (error "OP `~s` dont supported" (elt seq 0)))
  ;(log-info "before-read-line")
  (setf slen (read-line (uiop:process-info-output *python*)))
  ;(log-info "after-read-line")
  ;(format *slime-output* "~%slen = ~s" slen)
  (let ((len (ignore-errors (parse-integer slen))))
    (if (not len)
        (error "Bad parsing len: ~s" slen))
    (setf bytes-count (read-sequence seq (uiop:process-info-output *python*) :end len))
    ;(destroy-thread th)
    (if (zerop bytes-count)
        (values)
        (deserialize-python-value seq bytes-count)
        )
    )
  )

(defun py-eval (cmd)
  (send-to-python cmd "e"))

(defun py-exec (cmd)
  (send-to-python cmd "x"))

;;;;;;;;;;;;;;

;(let ((*python* *test-python*)) (write-to-python "e" "7"))

