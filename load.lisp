(defpackage :cl-to-python.init (:use :cl))
(in-package :cl-to-python.init)

(defun get-sym (str &aux
                      pos
                      (seps '(":" "::"))
                      (str-to-pkg (lambda (str) (find-package str)))
                      (str-to-sym (lambda (str pkg) (find-symbol str pkg))))
  
  (if (setf pos (search (first seps) str))
      (let ((pkg (funcall str-to-pkg (subseq str 0 pos))))
        (funcall str-to-sym (subseq str (1+ pos)) pkg))))
                                        ;(get-sym "CL:+")

(defun symcall (str-pkg-sym &rest args)
  (apply (get-sym str-pkg-sym) args))
;(symcall "CL:+" 1 2)

;(require 'asdf)

;(setf *load-pathname* #P"C:/last-files/my-projects/commad-of-carla/cl-to-python/load.lisp")
(defparameter *this-dir* (make-pathname :defaults *load-pathname* :type nil :name nil))
(push *this-dir* asdf:*central-registry*)

(symcall "QL:QUICKLOAD" :cl-to-python)

