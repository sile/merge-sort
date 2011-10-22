(defpackage merge-sort
  (:use common-lisp)
  (:shadow :common-lisp sort)
  (:export sort))
(in-package :merge-sort)

(declaim (inline halve last! merge-lists less-equal-than
                 sort2 sort3 sort4 sort5)
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
   
(defun less-equal-than (list1 list2 test key)
  (declare (function test key))
  (not (funcall test (funcall key (car list2)) (funcall key (car list1)))))

(defun merge-lists (list1 list2 test key)
  (declare (function test key))
  (labels ((recur (head tail l1 l2)
             (cond ((null l1) (cdr! tail l2) head)
                   ((null l2) (cdr! tail l1) head)
                   ((less-equal-than l1 l2 test key) 
                    (recur head (cdr! tail l1) (cdr l1) l2))
                   (t                 
                    (recur head (cdr! tail l2) l1 (cdr l2))))))
    (if (less-equal-than list1 list2 test key)
        (recur list1 list1 (cdr list1) list2)
      (recur list2 list2 list1 (cdr list2)))))

(defun sort2 (list test key &aux (l1 list) (l2 (cdr list)))
  (unless (less-equal-than l1 l2 test key)
    (rotatef (car l1) (car l2)))
  (values l1 (last! l2)))

(eval-when (:compile-toplevel)
  (defun symb (&rest args)
    (intern (format nil "~{~a~}" args))))

(macrolet ((vars (vars (list key) &body body)
             `(let* ,(loop FOR prev = nil THEN var
                           FOR var IN vars
                           FOR i fixnum FROM 0
                           COLLECT (if prev 
                                       `(,var (cdr ,prev))
                                     `(,var ,list)))
                (declare (function ,key))
                (let ,(loop FOR var IN vars
                            COLLECT `(,(symb '_ var) (funcall ,key (car ,var))))
                  ,@body)))
           (swap-if-greater-than (x y test)
             `(unless (less-equal-than ,x ,y ,test #'identity)
                (rotatef (car ,x) (car ,y))
                (rotatef ,(symb '_ x) ,(symb '_ y)))))

  (defun sort3 (list test key)
    (vars (a b c) (list key)
      (swap-if-greater-than a c test)
      (swap-if-greater-than a b test)
      (swap-if-greater-than b c test)
      (values a (last! c))))
  
  (defun sort4 (list test key)
    (vars (a b c d) (list key)
      (swap-if-greater-than a c test)
      (swap-if-greater-than b d test)
      (swap-if-greater-than a b test)
      (swap-if-greater-than c d test)
      (swap-if-greater-than b c test)
      (values a (last! d))))

  (defun sort5 (list test key)
    (vars (a b c d e) (list key)
      (swap-if-greater-than a b test)
      (swap-if-greater-than d e test)
      (swap-if-greater-than a c test)
      (swap-if-greater-than b c test)
      (swap-if-greater-than a d test)
      (swap-if-greater-than c d test)
      (swap-if-greater-than b e test)
      (swap-if-greater-than b c test)
      (swap-if-greater-than d e test)
      (values a (last! e)))))

(defun sort-impl (list size test key)
  (declare (fixnum size))
  (case size
    (5 (sort5 list test key))
    (4 (sort4 list test key))
    (3 (sort3 list test key))
    (t
     (multiple-value-let* (((size1 size2) (halve size))
                           ((list1 rest) (sort-impl list size1 test key))
                           ((list2 rest) (sort-impl rest size2 test key)))
       (values (merge-lists list1 list2 test key) rest)))))

(defun sort (list test &key (key #'identity) &aux (size (length list)))
  (declare (list list)
           (function test key)
           (optimize (speed 3) (safety 2) (debug 2)))
  (case size
    ((0 1) list)
    (2 
     (values (sort2 list test key)))
    (otherwise
     (values (sort-impl list size test key)))))
