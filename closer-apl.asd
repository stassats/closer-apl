;;; -*- Mode: Lisp -*-

(defsystem closer-apl
  :serial t
  :depends-on (named-readtables)
  :components ((:file "packages")
               (:file "closer-apl")
               (:file "functions")
               (:file "printing")))
