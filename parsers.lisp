#|
(setf str-ar "['c:\\last-files\\my-projects\\commad-of-carla\\cl-to-python', 'C:\\last-files\\sources\\carla-venv\\Scripts\\python37.zip', 'C:\\Users\\LinkFly\\AppData\\Local\\Programs\\Python\\Python37\\DLLs', 'C:\\Users\\LinkFly\\AppData\\Local\\Programs\\Python\\Python37\\lib', 'C:\\Users\\LinkFly\\AppData\\Local\\Programs\\Python\\Python37', 'C:\\last-files\\sources\\carla-venv', 'C:\\last-files\\sources\\carla-venv\\lib\\site-packages', 'C:\\last-files\\sources\\carla-venv\\lib\\site-packages\\win32', 'C:\\last-files\\sources\\carla-venv\\lib\\site-packages\\win32\\lib', 'C:\\last-files\\sources\\carla-venv\\lib\\site-packages\\Pythonwin', 'C:\\last-files\\sources\\carla\\PythonAPI\\examples']")
|#

(in-package :cl-to-python)

(defun parse-element (str)
  ;; this stub
  str)

(defun parse-array (str-ar)
  (setf str-ar (subseq str-ar 1 (1- (length str-ar))))
  (mapcar #'(lambda (str)
              (parse-element (string-trim " " str)))
          (uiop:split-string str-ar :separator ",")))
;(parse-array str-ar)


