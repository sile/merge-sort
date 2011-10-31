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

(declaim (ftype (function (list function function) list) sort-impl))
(defun sort-impl (list test key &aux (head (cons :head list)))
  (declare (optimize (speed 3) (safety 2) (debug 2)))
  (labels ((recur (size)
             (declare (optimize (speed 3) (safety 0) (debug 0))
                      (fixnum size)) 
             (if (= 1 size)
                 (let ((next (cdr head)))
                   (shiftf (cdr head) (cdr next) nil))
               (let ((half (ash size -1)))
                 (merge-lists head (recur half) (recur (- size half)) test key)))))
    (when list
      (recur (length list)))))

(defun sort (list test &key (key #'identity) inline)
  (declare (ignore inline))
  (sort-impl list test key))

(define-compiler-macro sort (&whole form list test &key (key '#'identity) inline)
  (if inline
      `(sort-impl ,list ,test ,key)
    form))
