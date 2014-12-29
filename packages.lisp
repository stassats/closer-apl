;;; -*- Mode: Lisp -*-

(defpackage #:closer-apl
  (:nicknames #:apl)
  (:use #:cl)
  (:export apl
           ∉
           ∣
           ∤
           ∛
           ∜
           ≉
           ≁

           |\|-reader|
           |{-reader|
           |[-reader|

           ⌊-READER
           ⌈-READER
           √-READER
           ¬-READER
           ∃-READER
           ∀-READER
           ∑-READER
           ∏-READER))
