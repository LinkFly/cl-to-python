(defsystem "cl-to-python"
  :depends-on ("flexi-streams" "bordeaux-threads" "uiop" "cl-ppcre" "uuid")
  :serial t
  :components ((:static-file "python-code" :pathname #P"my2.py")
               (:file "package")
               (:file "parsers")
               (:file "cl-to-python")
               )
  :in-order-to ((test-op (test-op "cl-to-python/tests"))))

(defsystem "cl-to-python/tests"
  :depends-on ("cl-to-python")
  :components ((:file "tests")))

