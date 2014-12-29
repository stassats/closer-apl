;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:closer-apl)

(defun ⌊-reader (stream char)
  (declare (ignore char))
  (prog1 `(floor ,(read stream t nil t))
    (let ((char (peek-char t stream t nil t)))
      (if (char= char #\⌋)
          (read-char stream t nil t)
          (error 'reader-error :stream stream)))))

(defun ⌈-reader (stream char)
  (declare (ignore char))
  (prog1 `(ceiling ,(read stream t nil t))
    (let ((char (peek-char t stream t nil t)))
      (if (char= char #\⌉)
          (read-char stream t nil t)
          (error 'reader-error :stream stream)))))

(defun |\|-reader| (stream char)
  (declare (ignore char))
  (prog1 `(abs ,(read stream t nil t))
    (let ((char (peek-char t stream t nil t)))
      (if (char= char #\|)
          (read-char stream t nil t)
          (error 'reader-error :stream stream)))))

(defun √-reader (stream char)
  (declare (ignore char))
  `(sqrt ,(read stream t nil t)))

(defun ¬-reader (stream char)
  (declare (ignore char))
  `(not ,(read stream t nil t)))

(defun ∉ (x list &rest args)
  ¬(apply #'member x list args))

(defun ∣ (a b)
  (= (rem a b) 0))

(defun ∤ (a b)
  (> (rem a b) 0))

(defun ∛ (x)
  (^ x 1/3))

(defun ∜ (x)
  (^ x 1/4))

(defun ≉ (x y)
  ¬(~ x y))

(defun ≁ (x y)
  ¬(≈ x y))


;;;
;;; ∃ n ∈ (1 2 3 4 5): (evenp n) => T

(defun ∃-reader (stream char)
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
    `(some (lambda (,variable) ,expression) ',list)))

(defun ∀-reader (stream char)
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
    `(every (lambda (,variable) ,expression) ',list)))

(defun |{-reader| (stream char)
  (declare (ignore char))
  (let* ((variable (read stream t nil t))
         (natural-p
          (progn
            (assert (char= #\∈ (peek-char t stream t nil t)))
            (read-char stream t nil t)
            (when (char= #\ℕ (peek-char t stream t nil t))
              (read-char stream t nil t)
              t)))
         (list
          (or natural-p
              (read stream t nil t)))
         (expression
          (progn
            (assert (char= #\: (peek-char t stream t nil t)))
            (read-char stream t nil t)
            (read stream t nil t))))
    (assert (char= #\} (peek-char t stream t nil t)))
    (read-char stream t nil t)
    (if natural-p
        `(loop for ,variable from 1
            while ,expression
            collect ,variable)
        `(loop for ,variable in ',list
            when ,expression
            collect ,variable))))

(defun ∑-reader (stream char)
  (declare (ignore char))
  (let* ((variable (read stream t nil t))
         (from (progn
                 (assert (char= #\= (peek-char t stream t nil t)))
                 (read-char stream t nil t)
                 (read stream t nil t)))
         (to (progn
               (assert (char= #\→ (peek-char t stream t nil t)))
               (read-char stream t nil t)
               (read stream t nil t)))
         (expression (read stream t nil t)))
    `(loop for ,variable from ,from to ,to
        sum ,expression)))

(defun ∏-reader  (stream char)
  (declare (ignore char))
  (let* ((variable (read stream t nil t))
         (from (progn
                 (assert (char= #\= (peek-char t stream t nil t)))
                 (read-char stream t nil t)
                 (read stream t nil t)))
         (to (progn
               (assert (char= #\→ (peek-char t stream t nil t)))
               (read-char stream t nil t)
               (read stream t nil t)))
         (expression (read stream t nil t))
         (result-sym (gensym)))
    `(loop for ,variable from ,from to ,to
        for ,result-sym = ,expression
        then (* ,result-sym ,expression)
        finally (return ,result-sym))))

(defun read-until (char stream)
  (with-output-to-string (out)
    (loop for input-char = (read-char stream)
	  until (char= char input-char)
	  do (write-char input-char out))))

(defun |[-reader| (stream char)
  (declare (ignore char))
  (let* ((string (read-until #\] stream))
         (comma (position #\, string))
         (dots (search ".." string))
         (start (parse-integer string :end (or comma dots)))
         (step (if comma
                   (- (parse-integer string :start (1+ comma)
                                     :end dots)
                      start)
                   1))
         (end (parse-integer string :start (+ dots 2))))
    (list 'quote
          (loop for i from start to end by step
             collect i))))

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
