(defpackage merge-sort
  (:use common-lisp)
  (:shadow :common-lisp sort)
  (:export sort))
(in-package :merge-sort)

(defun halve (size)
  (declare (fixnum size))
  (multiple-value-bind (n1 x) (floor size 2)
    (values (+ n1 x) n1)))

(defun merge-lists-recur (head list1 list2 test key)
  (declare (function test key))
  (labels ((recur (h l1 l2)
             (cond ((null l1) (setf (cdr h) l2))
                   ((null l2) (setf (cdr h) l1))
                   ((not (funcall test (funcall key (car l1)) 
                                  (funcall key (car l2))))
                    (recur (setf (cdr h) l2) l1 (cdr l2)))
                   (t
                    (recur (setf (cdr h) l1) (cdr l1) l2)))))
    (recur head list1 list2)
    (cdr head)))


;; TODO: 先頭にhead-consあり版も試す
(defun merge-lists (h head middle test key)
  (declare (function test key))
  (merge-lists-recur h head middle test key))

(defun sort-impl (head list size test key)
  (declare (function test key)
           (fixnum size))
  (case size
    (1 (let ((end (cdr list)))
         (setf (cdr list) nil)
         (values list end)))
    (otherwise
     (multiple-value-bind (size/fh size/lh) (halve size)
       (multiple-value-bind (list/fh list/lh)
                            (sort-impl head list size/fh test key)
         (multiple-value-bind (list/lh list/end)
                              (sort-impl head list/lh size/lh test key)
           ;; (print (list :ml list/fh list/lh list/end))
           (values (merge-lists head list/fh list/lh test key)
                   list/end)))))))

(defun sort (list test &key (key #'identity))
  (declare (list list))
  (let ((size (length list)))
    (when (plusp size)
      (values (sort-impl (list :head) list size test key)))))
