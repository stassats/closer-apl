;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:closer-apl)

(named-readtables:in-readtable :apl)

(set-macro-character #\⌋ (get-macro-character #\)))
(set-macro-character #\⌊
                     (lambda (stream char)
                       (declare (ignore char))
                       (prog1 `(floor ,(read stream t nil t))
                         (let ((char (peek-char t stream t nil t)))
                           (if (char= char #\⌋)
                               (read-char stream t nil t)
                               (error 'reader-error :stream stream))))))

(set-macro-character #\⌉ (get-macro-character #\)))
(set-macro-character #\⌈
                     (lambda (stream char)
                       (declare (ignore char))
                       (prog1 `(ceiling ,(read stream t nil t))
                         (let ((char (peek-char t stream t nil t)))
                           (if (char= char #\⌉)
                               (read-char stream t nil t)
                               (error 'reader-error :stream stream))))))

(set-macro-character #\√
                     (lambda (stream char)
                       (declare (ignore char))
                       `(sqrt ,(read stream t nil t))))
(set-macro-character #\¬
                     (lambda (stream char)
                       (declare (ignore char))
                       `(not ,(read stream t nil t))))

(set-macro-character #\≠ (constantly '/=))
(set-macro-character #\≤ (constantly '<=))
(set-macro-character #\≥ (constantly '>=))
(set-macro-character #\∅ (constantly ()))

;;; 
;;; ∃ n ∈ (1 2 3 4 5): (evenp n) => T

(set-macro-character #\∃
                     (lambda (stream char)
                       (declare (ignore char))
                       (let ((variable (read stream t nil t))
                             (list
                               (progn
                                 (assert (char= #\∈ (peek-char t stream t nil t)))
                                 (read-char stream t nil t)
                                 (read stream t nil t)))
                             (expression
                               (progn
                                 (assert (char= #\: (peek-char t stream t nil t)))
                                 (read-char stream t nil t)
                                 (read stream t nil t))))
                         `(some (lambda (,variable) ,expression) ',list))))

