(defpackage merge-sort
  (:use common-lisp)
  (:shadow :common-lisp sort)
  (:export sort))
(in-package :merge-sort)

(declaim (inline merge-lists sort-impl))

(defmacro cdr! (list new-cdr)
  `(setf (cdr ,list) ,new-cdr))
          
   
(defun merge-lists (head list1 list2 test key &aux (next (cdr head)))
  (labels ((less-equal-than (list1 list2)
             (not (funcall test (funcall key (car list2)) (funcall key (car list1)))))
           (recur (tail l1 l2)
             (cond ((null l1)               (cdr! tail l2) (shiftf (cdr head) next))
                   ((less-equal-than l1 l2) (recur (cdr! tail l1) (cdr l1) l2))
                   (t                       (recur (cdr! tail l2) (cdr l2) l1)))))
    (declare (inline less-equal-than))
    (recur head list1 list2)))

(defun sort-impl (list test key &aux (head (cons :head list)))
  (declare (list list)
           (function test key)
           (optimize (speed 3) (safety 2) (debug 2)))
  (labels ((recur (list size &aux (half1 (ash size -1)) (half2 (- size half1)))
             (declare (optimize (speed 3) (safety 0) (debug 0))
                      (fixnum size half1 half2))
             (if (= 1 size)
                 (shiftf (cdr head) (cdr list) nil)
               (merge-lists head (recur (cdr head) half1) (recur (cdr head) half2) test key))))
    (when list
      (values (recur list (length list))))))

(defun sort (list test &key (key #'identity) inline)
  (declare (ignore inline))
  (sort-impl list test key))

(define-compiler-macro sort (&whole form list test &key (key '#'identity) inline)
  (if inline
      `(sort-impl ,list ,test ,key)
    form))
