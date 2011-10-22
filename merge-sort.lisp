(defpackage merge-sort
  (:use common-lisp)
  (:shadow :common-lisp sort)
  (:export sort))
(in-package :merge-sort)

(declaim (inline halve last! merge-lists less-equal-than)
         (optimize (speed 3) (debug 0) (safety 0)))

(defun halve (n)
  (declare (fixnum n))
  (multiple-value-bind (n1 x) (floor n 2)
    (values (+ n1 x) n1)))

(defmacro cdr! (list new-cdr)
  `(setf (cdr ,list) ,new-cdr))

(defun last! (list)
  (prog1 (cdr list) (cdr! list nil)))

(defmacro multiple-value-let* (bind-specs &body body)
  (if (null bind-specs)
      `(locally ,@body)
    (destructuring-bind ((vars exp) . rest) bind-specs
      `(multiple-value-bind ,vars ,exp
         (multiple-value-let* ,rest ,@body)))))
   
(defun merge-lists (list1 list2 test key)
  (declare (function test key))
  (labels ((less-equal-than (list1 list2)
             (not (funcall test (funcall key (car list2)) (funcall key (car list1)))))
           (recur (head tail l1 l2)
             (cond ((null l1)               (cdr! tail l2) head)
                   ((null l2)               (cdr! tail l1) head)
                   ((less-equal-than l1 l2) (recur head (cdr! tail l1) (cdr l1) l2))
                   (t                       (recur head (cdr! tail l2) l1 (cdr l2))))))
    (declare (inline less-equal-than))
    (if (less-equal-than list1 list2)
        (recur list1 list1 (cdr list1) list2)
      (recur list2 list2 list1 (cdr list2)))))

(defun sort-impl (list size test key)
  (declare (fixnum size))
  (if (= 1 size)
      (values list (prog1 (cdr list) (cdr! list nil)))
    (multiple-value-let* (((size1 size2) (halve size))
                          ((list1 rest) (sort-impl list size1 test key))
                          ((list2 rest) (sort-impl rest size2 test key)))
      (values (merge-lists list1 list2 test key) rest))))

(defun sort (list test &key (key #'identity))
  (declare (list list)
           (function test key)
           (optimize (speed 3) (safety 2) (debug 2)))
  (when list
    (values (sort-impl list (length list) test key))))
