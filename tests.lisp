(in-package :cl-to-python)

(defparameter *test-python* nil)

(defun test ()
  (let ((*python* nil)
        (wait-res 3)
        (fact-res nil))
    ;(setf wait-res 3)
    (py-start)
    (setf *test-python* *python*)
    (setf fact-res (py-eval "1 + 2"))
    (if (/= wait-res fact-res)
        (progn (format t "~%TEST1 FAIL, fact-res = ~s, wait-res = ~s" fact-res wait-res)
               (py-stop)
               (return-from test)))
    (py-exec "tmp = 1 + 2")
    (setf fact-res (py-eval "tmp"))
    (if (/= wait-res fact-res)
        (progn (format t "~%TEST1 FAIL, fact-res = ~s, wait-res = ~s" fact-res wait-res)
               (py-stop)
               (return-from test)))
    (py-stop)
    (print "TESTS PASS")))

(defun time-test () (time (test)))

(time-test)

