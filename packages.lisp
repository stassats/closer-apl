;;; -*- Mode: Lisp -*-

(defpackage #:closer-apl
  (:nicknames #:apl)
  (:use #:cl)
  (:export ))

(named-readtables:defreadtable :apl
  (:merge :standard))
