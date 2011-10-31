(defpackage merge-sort
  (:use common-lisp)
  (:shadow :common-lisp sort)
  (:export sort))
(in-package :merge-sort)

(declaim (inline merge-lists sort-impl))

(defun merge-lists (head list1 list2 test key &aux (next (cdr head)) (tail head))
  (macrolet ((less-equal-than (l1 l2)
               `(not (funcall test (funcall key (car ,l2)) (funcall key (car ,l1)))))
             (merge-one (l1 l2)
               `(unless (setf ,l1 (cdr (setf tail (setf (cdr tail) ,l1))))
                  (setf (cdr tail) ,l2)
                  (return (shiftf (cdr head) next)))))
    (loop
     (if (less-equal-than list1 list2)
         (merge-one list1 list2)
       (merge-one list2 list1)))))

(declaim (ftype (function (list function function) list) sort-impl))
(defun sort-impl (list test key &aux (head (cons :head list)))
  (declare (optimize (speed 3) (safety 2) (debug 2))
           (dynamic-extent head))
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
