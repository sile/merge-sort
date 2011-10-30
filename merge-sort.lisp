(defpackage merge-sort
  (:use common-lisp)
  (:shadow :common-lisp sort)
  (:export sort))
(in-package :merge-sort)

(declaim (inline halve merge-lists sort-impl))

(defun halve (n)
  (declare (fixnum n))
  (multiple-value-bind (n1 x) (floor n 2)
    (values (+ n1 x) n1)))

(defmacro cdr! (list new-cdr)
  `(setf (cdr ,list) ,new-cdr))

(defmacro multiple-value-let* (bind-specs &body body)
  (if (null bind-specs)
      `(locally ,@body)
    (destructuring-bind ((vars exp) . rest) bind-specs
      `(multiple-value-bind ,vars ,exp
         (multiple-value-let* ,rest ,@body)))))
   
(defun merge-lists (list1 list2 test key)
  (labels ((less-equal-than (list1 list2)
             (not (funcall test (funcall key (car list2)) (funcall key (car list1)))))
           (recur (head tail l2 &aux (l1 (cdr tail)))
             (cond ((null l1)                (cdr! tail l2) head)
                   ((less-equal-than l1 l2)  (recur head l1 l2))
                   (t                        (recur head (cdr! tail l2) l1)))))
    (declare (inline less-equal-than))
    (if (less-equal-than list1 list2)
        (recur list1 list1 list2)
      (recur list2 list2 list1))))

(defun sort-impl (list test key)
  (declare (list list)
           (function test key)
           (optimize (speed 3) (safety 2) (debug 2)))
  (labels ((recur (list size)
             (declare (optimize (speed 3) (safety 0) (debug 0)))
             (if (= 1 size)
                 (values list (prog1 (cdr list) (cdr! list nil)))
               (multiple-value-let* (((size1 size2) (halve size))
                                     ((list1 rest) (recur list size1))
                                     ((list2 rest) (recur rest size2)))
                 (values (merge-lists list1 list2 test key) rest)))))
    (when list
      (values (recur list (length list))))))

(defun sort (list test &key (key #'identity) inline)
  (declare (ignore inline))
  (sort-impl list test key))

(define-compiler-macro sort (&whole form list test &key (key '#'identity) inline)
  (if inline
      `(sort-impl ,list ,test ,key)
    form))
