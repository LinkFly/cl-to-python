
(defpackage "cl-to-python.init" (:use :cl))
(in-package "cl-to-python.init")

(require 'asdf)

;(setf *load-pathname* #P"C:/last-files/my-projects/commad-of-carla/cl-to-python/load.lisp")
(defparameter *this-dir* (make-pathname :defaults *load-pathname* :type nil :name nil))
(push *this-dir* asdf:*central-registry*)

(asdf:load-system :cl-to-python)

