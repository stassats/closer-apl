;;; -*- Mode: Lisp -*-

(defpackage #:closer-apl
  (:nicknames #:apl)
  (:use #:cl)
  (:export :apl
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

(flet ((rdelim (stream char)
         (declare (ignore stream char))
         (get-macro-character #\))))
  (named-readtables:defreadtable :apl
    (:merge :standard)
    (:macro-char #\⌋ #'rdelim)
    (:macro-char #\⌊ #'⌊-reader)
    (:macro-char #\⌉ #'rdelim)
    (:macro-char #\⌈ #'⌈-reader)
    (:macro-char #\| #'rdelim)
    (:macro-char #\| #'|\|-reader|)
    (:macro-char #\√ #'√-reader)
    (:macro-char #\¬ #'¬-reader)

    (:macro-char #\≠ (constantly '/=))
    (:macro-char #\≤ (constantly '<=))
    (:macro-char #\≥ (constantly '>=))
    (:macro-char #\⊨ (constantly t))
    (:macro-char #\∅ (constantly ()))
    (:macro-char #\∈ (constantly 'member))
    (:macro-char #\⋀ (constantly 'and))
    (:macro-char #\⋁ (constantly 'or))
    (:macro-char #\⋃ (constantly 'append))
    (:macro-char #\⊂ (constantly 'subsetp ))
    (:macro-char #\~ (constantly 'equal))
    (:macro-char #\≈ (constantly 'equalp))
    (:macro-char #\^ (constantly 'expt))
    (:macro-char #\→ (constantly 'setf))

    (:macro-char #\∃ #'∃-reader)
    (:macro-char #\∀ #'∀-reader)
    (:macro-char #\{ #'|{-reader|)
    (:macro-char #\∑ #'∑-reader)
    (:macro-char #\∏ #'∏-reader)
    (:macro-char #\[ #'|[-reader|)))
