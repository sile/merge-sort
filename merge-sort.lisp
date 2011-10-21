(defpackage merge-sort
  (:use common-lisp)
  (:shadow :common-lisp sort)
  (:export sort))
(in-package :merge-sort)

(defun halve (size)
  (declare (fixnum size))
  (multiple-value-bind (n1 x) (floor size 2)
    (values (+ n1 x) n1)))

(defun merge-lists-recur (head list1 list2 test key &aux (h head)) ; XXX: h
  (declare (function test key))
  (labels ((set-next (list)
             (setf (cdr head) list
                   head list))
           (recur (l1 l2)
             (cond ((null l1)
                    (set-next l2))
                   ((null l2)
                    (set-next l1))
                   ((not (funcall test (funcall key (car l1)) 
                                  (funcall key (car l2))))
                    (set-next l2)
                    (recur l1 (cdr l2)))
                   (t
                    (set-next l1)
                    (recur (cdr l1) l2)))))
    (recur list1 list2)
    h))

(defun merge-lists (head middle test key)
  (declare (function test key))
    (if (funcall test (funcall key (first head)) (funcall key (first middle)))
        (merge-lists-recur head (cdr head) middle test key)
      (merge-lists-recur middle head (cdr middle) test key)))

(defun sort-impl (list size test key)
  (declare (function test key)
           (fixnum size))
  (case size
    (1 (let ((end (cdr list)))
         (setf (cdr list) nil)
         (values list end)))
    (otherwise
     (multiple-value-bind (size/fh size/lh) (halve size)
       (multiple-value-bind (list/fh list/lh)
                            (sort-impl list size/fh test key)
         (multiple-value-bind (list/lh list/end)
                              (sort-impl list/lh size/lh test key)
           ;; (print (list :ml list/fh list/lh list/end))
           (values (merge-lists list/fh list/lh test key)
                   list/end)))))))

(defun sort (list test &key (key #'identity))
  (declare (list list))
  (let ((size (length list)))
    (when (plusp size)
      (values (sort-impl list size test key)))))
