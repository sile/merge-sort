(defpackage merge-sort
  (:use common-lisp)
  (:shadow :common-lisp sort)
  (:export sort))
(in-package :merge-sort)

(defun halve (size)
  (declare (fixnum size))
  (multiple-value-bind (n1 x) (floor size 2)
    (values (+ n1 x) n1)))

(defun merge-lists (list1 list2 n1 n2 test key)
  (declare (function test key)
           (fixnum n1 n2))
  (labels ((recur (h l1 l2 n1 n2)
             (cond ((zerop n1) 
                    (setf (cdr h) l2)
                    (nthcdr n2 l2))
                   ((zerop n2)
                    (setf (cdr h) l1
                          (cdr (nthcdr (1- n1) l1)) l2)
                      l2)
                   ((not (funcall test (funcall key (car l1)) (funcall key (car l2))))
                    (recur (setf (cdr h) l2) l1 (cdr l2) n1 (1- n2)))
                   (t
                    (recur (setf (cdr h) l1) (cdr l1) l2 (1- n1) n2)))))
    (if (funcall test (funcall key (first list1)) (funcall key (first list2)))
        (values list1 (recur list1 (cdr list1) list2 (1- n1) n2))
      (values list2 (recur list2 list1 (cdr list2) n1 (1- n2))))))

(defun sort-impl (list size test key)
  (declare (function test key)
           (fixnum size))
  (case size
    (1 (values list (cdr list)))
    (otherwise
     (multiple-value-bind (size/fh size/lh) (halve size)
       (multiple-value-bind (list/fh list/lh)
                            (sort-impl list size/fh test key)
         (let ((list/lh (sort-impl list/lh size/lh test key)))
           (merge-lists list/fh list/lh size/fh size/lh test key)))))))

(defun sort (list test &key (key #'identity))
  (declare (list list))
  (let ((size (length list)))
    (when (plusp size)
      (values (sort-impl list size test key)))))
