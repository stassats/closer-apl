(in-package #:closer-apl)

(named-readtables:in-readtable :apl)

(set-pprint-dispatch
 'null
 (lambda (stream object)
   (declare (ignore object))
   (write-char #\∅ stream)))

(set-pprint-dispatch
 '(eql ⊨)
 (lambda (stream object)
   (declare (ignore object))
   (write-char #\⊨ stream)))

