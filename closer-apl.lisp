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

(set-macro-character #\| (get-macro-character #\)))
(set-macro-character #\|
                     (lambda (stream char)
                       (declare (ignore char))
                       (prog1 `(abs ,(read stream t nil t))
                         (let ((char (peek-char t stream t nil t)))
                           (if (char= char #\|)
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
(set-macro-character #\⊨ (constantly t))
(set-macro-character #\∅ (constantly ()))
(set-macro-character #\∈ (constantly 'member))
(set-macro-character #\⋀ (constantly 'and))
(set-macro-character #\⋁ (constantly 'or))
(set-macro-character #\⋃ (constantly 'append))
(set-macro-character #\⊂ (constantly 'subsetp ))
(set-macro-character #\~ (constantly 'equal))
(set-macro-character #\≈ (constantly 'equalp))
(set-macro-character #\^ (constantly 'expt))
(set-macro-character #\→ (constantly 'setf))


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
   ¬ (≈ x y))


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

(set-macro-character #\∀
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
                         `(every (lambda (,variable) ,expression) ',list))))

(set-macro-character #\{
                     (lambda (stream char)
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
                                    collect ,variable)))))

(set-macro-character #\∑
                     (lambda (stream char)
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
                                sum ,expression))))

(set-macro-character #\∏
                     (lambda (stream char)
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
                                finally (return ,result-sym)))))
